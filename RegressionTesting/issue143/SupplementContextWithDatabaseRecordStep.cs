// <copyright file="SupplementContextWithDatabaseRecordStep.cs" company="Chris Trout">
// MIT License
//
// Copyright(c) 2020-2021 Chris Trout
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.
// </copyright>

namespace MyTrout.Pipelines.Steps.Data
{
  using Dapper;
  using Microsoft.Extensions.Logging;
  using System;
  using System.Collections.Generic;
  using System.Data.Common;
  using System.Globalization;
  using System.Threading.Tasks;

  /// <summary>
  /// Adds additiona data to <see cref="IPipelineContext"/> from a query run against a database.
  /// </summary>
  public class SupplementContextWithDatabaseRecordStep : AbstractPipelineStep<SupplementContextWithDatabaseRecordStep, SupplementContextWithDatabaseRecordOptions>
  {
    /// <summary>
    /// Initializes a new instance of the <see cref="SupplementContextWithDatabaseRecordStep"/> class with the specified options.
    /// </summary>
    /// <param name="logger">The logger for this step.</param>
    /// <param name="providerFactory">An <see cref="DbProviderFactory"/> instance.</param>
    /// <param name="next">The next step in the pipeline.</param>
    /// <param name="options">Step-specific options for altering behavior.</param>
    public SupplementContextWithDatabaseRecordStep(ILogger<SupplementContextWithDatabaseRecordStep> logger, DbProviderFactory providerFactory, SupplementContextWithDatabaseRecordOptions options, IPipelineRequest next)
        : base(logger, options, next)
    {
      this.ProviderFactory = providerFactory ?? throw new ArgumentNullException(nameof(providerFactory));
    }

    /// <summary>
    /// Gets a <see cref="DbProviderFactory"/> instance.
    /// </summary>
    public DbProviderFactory ProviderFactory { get; }

    /// <summary>
    /// Reads one record from the database, supplements the <paramref name="context"/> and restores the original values once downstream processing is completed.
    /// </summary>
    /// <param name="context">The pipeline context.</param>
    /// <returns>A completed <see cref="Task" />.</returns>
    protected override async Task InvokeCoreAsync(IPipelineContext context)
    {
      DynamicParameters parameters = new DynamicParameters();

      foreach (var parameterName in this.Options.SqlStatement.ParameterNames)
      {
        parameters.Add(parameterName, context.Items[parameterName]);
      }

      List<string> addedFields = new List<string>();
      Dictionary<string, object> previousValues = new Dictionary<string, object>();

      bool hasProcessedOneRecord = false;

      using (var connection = this.ProviderFactory.CreateConnection())
      {
        //connection.AssertValueIsNotNull(() => Resources.CONNECTION_IS_NULL(this.ProviderFactory.GetType().Name));

#pragma warning disable CS8602 // AssertValueIsNotNull guarantees a non-null value here.

        connection.ConnectionString = await this.Options.RetrieveConnectionStringAsync.Invoke().ConfigureAwait(false);

        await connection.OpenAsync().ConfigureAwait(false);

        using (var reader = await connection.ExecuteReaderAsync(this.Options.SqlStatement.Statement, param: parameters, commandType: this.Options.SqlStatement.CommandType).ConfigureAwait(false))
        {
          if (await reader.ReadAsync().ConfigureAwait(false))
          {
            hasProcessedOneRecord = true;

            for (int index = 0; index < reader.FieldCount; index++)
            {
              string fieldName = reader.GetName(index);
              if (context.Items.ContainsKey(fieldName))
              {
                // Cache the previous values to restore them after the "next" processing is completed.
                previousValues.Add(fieldName, context.Items[fieldName]);

                // Allows the value to be added back to reduce Cognitive Complexity.
                context.Items.Remove(fieldName);
              }

              context.Items.Add(fieldName, reader.GetValue(index));
              addedFields.Add(fieldName);
            }
          }
          else
          {
            context.Errors.Add(new InvalidOperationException());
            //                          Resources.NO_DATA_FOUND(CultureInfo.CurrentCulture, nameof(SupplementContextWithDatabaseRecordStep))));
          }
        }

#pragma warning restore CS8602 // AssertValueIsNotNull guarantees a non-null value here.
      } // Closed the connection to prevent any resource leakage into later steps.

      if (hasProcessedOneRecord)
      {
        await this.Next.InvokeAsync(context).ConfigureAwait(false);

        // Remove all of the field values that were added or altered.
        foreach (string field in addedFields)
        {
          context.Items.Remove(field);
        }

        // Add the previous values back.
        foreach (KeyValuePair<string, object> field in previousValues)
        {
          context.Items.Add(field.Key, field.Value);
        }
      }
    }
  }
}
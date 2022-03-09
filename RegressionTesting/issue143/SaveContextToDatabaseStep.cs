// <copyright file="SaveContextToDatabaseStep.cs" company="Chris Trout">
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
  using System.Data.Common;
  using System.Globalization;
  using System.Linq;
  using System.Threading.Tasks;

  /// <summary>
  /// Adds additiona data to <see cref="IPipelineContext"/> from a query run against a database.
  /// </summary>
  public class SaveContextToDatabaseStep : AbstractPipelineStep<SaveContextToDatabaseStep, SaveContextToDatabaseOptions>
  {
    /// <summary>
    /// Initializes a new instance of the <see cref="SaveContextToDatabaseStep"/> class with the specified options.
    /// </summary>
    /// <param name="logger">The logger for this step.</param>
    /// <param name="providerFactory">An <see cref="DbProviderFactory"/> instance.</param>
    /// <param name="next">The next step in the pipeline.</param>
    /// <param name="options">Step-specific options for altering behavior.</param>
    public SaveContextToDatabaseStep(ILogger<SaveContextToDatabaseStep> logger, DbProviderFactory providerFactory, SaveContextToDatabaseOptions options, IPipelineRequest next)
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
      await this.Next.InvokeAsync(context).ConfigureAwait(false);

      context.AssertStringIsNotWhiteSpace(DatabaseConstants.DATABASE_STATEMENT_NAME);

      var sqlName = context.Items[DatabaseConstants.DATABASE_STATEMENT_NAME] as string;
      var sql = this.Options.SqlStatements.FirstOrDefault(x => x.Name == sqlName);

      //sql.AssertValueIsNotNull(() => Resources.SQL_STATEMENT_NOT_FOUND(CultureInfo.CurrentCulture, sqlName));

#pragma warning disable CS8602 // AssertValueIsNotNull guarantees a non-null value here.

      DynamicParameters parameters = new DynamicParameters();

      foreach (var parameterName in sql.ParameterNames)
      {
        parameters.Add(parameterName, context.Items[parameterName]);
      }

      // Removed using block to determine if OpenCover can correctly determine code coverage on this block.
#pragma warning disable CS8632 // The annotation for nullable reference types should only be used in code within a '#nullable' annotations context.
      DbConnection? connection = null;
#pragma warning restore CS8632 // The annotation for nullable reference types should only be used in code within a '#nullable' annotations context.
      using (connection = this.ProviderFactory.CreateConnection())
      {
        //connection.AssertValueIsNotNull(() => Resources.CONNECTION_IS_NULL(this.ProviderFactory.GetType().Name));

        connection.ConnectionString = await this.Options.RetrieveConnectionStringAsync.Invoke().ConfigureAwait(false);

        await connection.OpenAsync().ConfigureAwait(false);

        int result = await connection.ExecuteAsync(sql.Statement, param: parameters, commandType: sql.CommandType).ConfigureAwait(false);

#pragma warning restore CS8602

        context.Items.Add(DatabaseConstants.DATABASE_ROWS_AFFECTED, result);
      }

      await Task.CompletedTask.ConfigureAwait(false);
    }
  }
}
// <copyright file="SaveContextToDatabaseStepTests.cs" company="Chris Trout">
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

namespace MyTrout.Pipelines.Steps.Data.Tests
{
  using Dapper;
  using Microsoft.Extensions.Logging;
  using Microsoft.VisualStudio.TestTools.UnitTesting;
  using Moq;
  using MyTrout.Pipelines.Core;
  using System;
  using System.Collections.Generic;
  using System.Data.Common;
  using System.Data.SqlClient;
  using System.Globalization;
  using System.Linq;
  using System.Runtime.InteropServices;
  using System.Threading.Tasks;

  [TestClass]
  [System.Diagnostics.CodeAnalysis.ExcludeFromCodeCoverage]
  public class SaveContextToDatabaseStepTests
  {
    [TestMethod]
    public void Constructs_SaveContextToDatabaseStep_Successfully()
    {
      // arrange
      ILogger<SaveContextToDatabaseStep> logger = new Mock<ILogger<SaveContextToDatabaseStep>>().Object;
      var providerFactory = new Mock<DbProviderFactory>().Object;
      var options = new SaveContextToDatabaseOptions();
      IPipelineRequest next = new Mock<IPipelineRequest>().Object;

      // act
      var result = new SaveContextToDatabaseStep(logger, providerFactory, options, next);

      // assert
      Assert.IsNotNull(result);
      Assert.AreEqual(logger, result.Logger);
      Assert.AreEqual(next, result.Next);
      Assert.AreEqual(options, result.Options);
      Assert.AreEqual(providerFactory, result.ProviderFactory);
    }

    [TestMethod]
    public async Task Returns_PipelineContext_Error_From_InvokeAsync_When_DbProviderFactory_CreateConnection_Returns_Null()
    {
      // arrange
      var logger = new Mock<ILogger<SaveContextToDatabaseStep>>().Object;

      var providerFactoryMock = new Mock<DbProviderFactory>();
      providerFactoryMock.Setup(x => x.CreateConnection()).Returns(null as DbConnection);
      var providerFactory = providerFactoryMock.Object;
      var environmentVariableTarget = RuntimeInformation.IsOSPlatform(OSPlatform.Windows) ? EnvironmentVariableTarget.Machine : EnvironmentVariableTarget.Process;

      var options = new SaveContextToDatabaseOptions()
      {
        SqlStatements = new List<SqlStatement>()
        {
          new SqlStatement()
          {
              CommandType = System.Data.CommandType.StoredProcedure,
              Name = "StatementName",
              Statement = "UPDATE dbo.Cartoon SET Description = 'Nom, nom, nom' WHERE CartoonId > 1;",
          }
        },

        RetrieveConnectionStringAsync = () => { return Task.FromResult(Environment.GetEnvironmentVariable("PIPELINE_TEST_AZURE_SQL_SERVER_CONNECTION_STRING", environmentVariableTarget)); }
      };

      var context = new PipelineContext();
      context.Items.Add(DatabaseConstants.DATABASE_STATEMENT_NAME, "StatementName");

      IPipelineRequest next = new Mock<IPipelineRequest>().Object;

      var sut = new SaveContextToDatabaseStep(logger, providerFactory, options, next);

      int expectedErrorCount = 1;
      string expectedMessage = "expected"; //Resources.CONNECTION_IS_NULL(CultureInfo.CurrentCulture, providerFactory.GetType().Name);

      // act
      await sut.InvokeAsync(context).ConfigureAwait(false);

      // assert
      Assert.AreEqual(expectedErrorCount, context.Errors.Count);
      Assert.IsInstanceOfType(context.Errors[0], typeof(InvalidOperationException));
      Assert.AreEqual(expectedMessage, context.Errors[0].Message);
    }

    [TestMethod]
    public async Task Returns_PipelineContext_Error_From_InvokeAsync_When_Invalid_Query_Is_Provided()
    {
      // arrange
      ILogger<SaveContextToDatabaseStep> logger = new Mock<ILogger<SaveContextToDatabaseStep>>().Object;
      var providerFactory = SqlClientFactory.Instance;
      var environmentVariableTarget = RuntimeInformation.IsOSPlatform(OSPlatform.Windows) ? EnvironmentVariableTarget.Machine : EnvironmentVariableTarget.Process;
      var options = new SaveContextToDatabaseOptions()
      {
        SqlStatements = new List<SqlStatement>()
        {
          new SqlStatement()
          {
              CommandType = System.Data.CommandType.StoredProcedure,
              Name = "StatementName",
              Statement = "UPDATE dbo.Cartoon SET Description = 'Nom, nom, nom' WHERE CartoonId > 1;",
          }
        },

        RetrieveConnectionStringAsync = () => { return Task.FromResult(Environment.GetEnvironmentVariable("PIPELINE_TEST_AZURE_SQL_SERVER_CONNECTION_STRING", environmentVariableTarget)); }
      };

      using (var connection = providerFactory.CreateConnection())
      {
        connection.ConnectionString = options.RetrieveConnectionStringAsync.Invoke().Result;
        await connection.ExecuteAsync("dbo.CartoonInsert", new { CartoonId = 2, Name = "Road Runner", Description = null as string }, commandType: System.Data.CommandType.StoredProcedure).ConfigureAwait(false);
        await connection.ExecuteAsync("dbo.CartoonInsert", new { CartoonId = 3, Name = "Wile E. Coyote", Description = null as string }, commandType: System.Data.CommandType.StoredProcedure).ConfigureAwait(false);
      }

      var context = new PipelineContext();
      context.Items.Add(DatabaseConstants.DATABASE_STATEMENT_NAME, "StatementName");

      IPipelineRequest next = new Mock<IPipelineRequest>().Object;

      var sut = new SaveContextToDatabaseStep(logger, providerFactory, options, next);

      int expectedErrorCount = 1;
      string expectedMessage = "Could not find stored procedure 'UPDATE dbo.Cartoon SET Description = 'Nom, nom, nom' WHERE CartoonId > 1'.";

      // act
      await sut.InvokeAsync(context).ConfigureAwait(false);

      // assert
      try
      {
        Assert.AreEqual(expectedErrorCount, context.Errors.Count);
        Assert.AreEqual(expectedMessage, context.Errors[0].Message);
      }
      finally
      {
        // cleanup
#pragma warning disable IDE0063 // Use simple 'using' statement
        using (var connection = providerFactory.CreateConnection())
#pragma warning restore IDE0063 // Use simple 'using' statement
        {
          connection.ConnectionString = options.RetrieveConnectionStringAsync.Invoke().Result;
          await connection.ExecuteAsync("DELETE FROM dbo.Cartoon WHERE CartoonId = @CartoonId;", new { CartoonId = 2 }, commandType: System.Data.CommandType.Text).ConfigureAwait(false);
          await connection.ExecuteAsync("DELETE FROM dbo.Cartoon WHERE CartoonId = @CartoonId;", new { CartoonId = 3 }, commandType: System.Data.CommandType.Text).ConfigureAwait(false);
        }
      }
    }

    [TestMethod]
    public async Task Returns_PipelineContext_Error_From_InvokeAsync_When_SqlStatement_Is_Not_Found()
    {
      // arrange
      var logger = new Mock<ILogger<SaveContextToDatabaseStep>>().Object;
      var providerFactory = SqlClientFactory.Instance;
      var environmentVariableTarget = RuntimeInformation.IsOSPlatform(OSPlatform.Windows) ? EnvironmentVariableTarget.Machine : EnvironmentVariableTarget.Process;
      var options = new SaveContextToDatabaseOptions()
      {
        SqlStatements = new List<SqlStatement>()
                {
                    new SqlStatement()
                    {
                        CommandType = System.Data.CommandType.StoredProcedure,
                        Name = "StatementName-1",
                        Statement = "UPDATE dbo.Cartoon SET Description = 'Nom, nom, nom' WHERE CartoonId > 1;",
                    }
                },

        RetrieveConnectionStringAsync = () => { return Task.FromResult(Environment.GetEnvironmentVariable("PIPELINE_TEST_AZURE_SQL_SERVER_CONNECTION_STRING", environmentVariableTarget)); }
      };

      string missingStatementName = "OASODUJAFPIIO09asd09uoiufhojkshdf";
      var context = new PipelineContext();
      context.Items.Add(DatabaseConstants.DATABASE_STATEMENT_NAME, missingStatementName);

      IPipelineRequest next = new Mock<IPipelineRequest>().Object;

      var sut = new SaveContextToDatabaseStep(logger, providerFactory, options, next);

      int expectedErrorCount = 1;
      string expectedMessage = "expected"; // Resources.SQL_STATEMENT_NOT_FOUND(CultureInfo.CurrentCulture, missingStatementName);

      // act
      await sut.InvokeAsync(context).ConfigureAwait(false);

      // assert
      Assert.AreEqual(expectedErrorCount, context.Errors.Count);
      Assert.AreEqual(expectedMessage, context.Errors[0].Message);
    }

    [TestMethod]
    public async Task Returns_DatabaseRecordsAffected_From_InvokeAsync_When_Multiple_Records_Are_Changed()
    {
      // arrange
      var logger = new Mock<ILogger<SaveContextToDatabaseStep>>().Object;
      var providerFactory = SqlClientFactory.Instance;
      var environmentVariableTarget = RuntimeInformation.IsOSPlatform(OSPlatform.Windows) ? EnvironmentVariableTarget.Machine : EnvironmentVariableTarget.Process;
      var options = new SaveContextToDatabaseOptions()
      {
        SqlStatements = new List<SqlStatement>()
                {
                    new SqlStatement()
                    {
                        CommandType = System.Data.CommandType.Text,
                        Name = "StatementName",
                        ParameterNames = new List<string>() { "CartoonId" },
                        Statement = "UPDATE dbo.Cartoon SET Description = 'Nom, nom, nom' WHERE CartoonId > @CartoonId;",
                    }
                },
        RetrieveConnectionStringAsync = () => { return Task.FromResult(Environment.GetEnvironmentVariable("PIPELINE_TEST_AZURE_SQL_SERVER_CONNECTION_STRING", environmentVariableTarget)); }
      };

      using (var connection = providerFactory.CreateConnection())
      {
        connection.ConnectionString = options.RetrieveConnectionStringAsync.Invoke().Result;
        await connection.ExecuteAsync("dbo.CartoonInsert", new { CartoonId = 2, Name = "Road Runner", Description = null as string }, commandType: System.Data.CommandType.StoredProcedure).ConfigureAwait(false);
        await connection.ExecuteAsync("dbo.CartoonInsert", new { CartoonId = 3, Name = "Wile E. Coyote", Description = null as string }, commandType: System.Data.CommandType.StoredProcedure).ConfigureAwait(false);
      }

      var context = new PipelineContext();
      context.Items.Add("CartoonId", 1);
      context.Items.Add(DatabaseConstants.DATABASE_STATEMENT_NAME, "StatementName");

      IPipelineRequest next = new Mock<IPipelineRequest>().Object;

      var sut = new SaveContextToDatabaseStep(logger, providerFactory, options, next);

      int expectedAffectedRecords = 2;

      // act
      await sut.InvokeAsync(context).ConfigureAwait(false);

      // assert
      try
      {
        if (context.Errors.Any())
        {
          throw context.Errors[0];
        }

        // Checking to make sure that the Context was restored to its original state after the Step execution is completed.
        Assert.IsTrue(context.Items.ContainsKey(DatabaseConstants.DATABASE_ROWS_AFFECTED));
        Assert.AreEqual(expectedAffectedRecords, context.Items[DatabaseConstants.DATABASE_ROWS_AFFECTED]);
      }
      finally
      {
        // cleanup
#pragma warning disable IDE0063 // Use simple 'using' statement
        using (var connection = providerFactory.CreateConnection())
#pragma warning restore IDE0063 // Use simple 'using' statement
        {
          connection.ConnectionString = options.RetrieveConnectionStringAsync.Invoke().Result;
          await connection.ExecuteAsync("DELETE FROM dbo.Cartoon WHERE CartoonId = @CartoonId;", new { CartoonId = 2 }, commandType: System.Data.CommandType.Text).ConfigureAwait(false);
          await connection.ExecuteAsync("DELETE FROM dbo.Cartoon WHERE CartoonId = @CartoonId;", new { CartoonId = 3 }, commandType: System.Data.CommandType.Text).ConfigureAwait(false);
        }
      }
    }

    [TestMethod]
    public void Throw_ArgumentNullException_From_Constructor_When_Logger_Is_Null()
    {
      // arrange
      ILogger<SaveContextToDatabaseStep> logger = null;
      DbProviderFactory providerFactory = new Mock<DbProviderFactory>().Object;
      var options = new SaveContextToDatabaseOptions();
      IPipelineRequest next = new Mock<IPipelineRequest>().Object;

      string expectedParamName = nameof(logger);

      // act
      var result = Assert.ThrowsException<ArgumentNullException>(() => new SaveContextToDatabaseStep(logger, providerFactory, options, next));

      // assert
      Assert.IsNotNull(result);
      Assert.AreEqual(expectedParamName, result.ParamName);
    }

    [TestMethod]
    public void Throws_ArgumentNullException_From_Constructor_When_Next_Is_Null()
    {
      // arrange
      ILogger<SaveContextToDatabaseStep> logger = new Mock<ILogger<SaveContextToDatabaseStep>>().Object;
      DbProviderFactory providerFactory = new Mock<DbProviderFactory>().Object;
      var options = new SaveContextToDatabaseOptions();
      IPipelineRequest next = null;

      string expectedParamName = nameof(next);

      // act
      var result = Assert.ThrowsException<ArgumentNullException>(() => new SaveContextToDatabaseStep(logger, providerFactory, options, next));

      // assert
      Assert.IsNotNull(result);
      Assert.AreEqual(expectedParamName, result.ParamName);
    }

    [TestMethod]
    public void Throws_ArgumentNullException_From_Constructor_When_Options_Is_Null()
    {
      // arrange
      ILogger<SaveContextToDatabaseStep> logger = new Mock<ILogger<SaveContextToDatabaseStep>>().Object;
      DbProviderFactory providerFactory = new Mock<DbProviderFactory>().Object;
      SaveContextToDatabaseOptions options = null;
      IPipelineRequest next = new Mock<IPipelineRequest>().Object;

      string expectedParamName = nameof(options);

      // act
      var result = Assert.ThrowsException<ArgumentNullException>(() => new SaveContextToDatabaseStep(logger, providerFactory, options, next));

      // assert
      Assert.IsNotNull(result);
      Assert.AreEqual(expectedParamName, result.ParamName);
    }

    [TestMethod]
    public void Throws_ArgumentNullException_From_Constructor_When_ProviderFactory_Is_Null()
    {
      // arrange
      ILogger<SaveContextToDatabaseStep> logger = new Mock<ILogger<SaveContextToDatabaseStep>>().Object;
      DbProviderFactory providerFactory = null;
      var options = new SaveContextToDatabaseOptions();
      IPipelineRequest next = new Mock<IPipelineRequest>().Object;

      string expectedParamName = nameof(providerFactory);

      // act
      var result = Assert.ThrowsException<ArgumentNullException>(() => new SaveContextToDatabaseStep(logger, providerFactory, options, next));

      // assert
      Assert.IsNotNull(result);
      Assert.AreEqual(expectedParamName, result.ParamName);
    }

    [TestMethod]
    public async Task Throw_ArgumentNullException_From_InvokeCoreAsync_When_IPipelineContext_Is_Null()
    {
      // arrange
      ILogger<SaveContextToDatabaseStep> logger = new Mock<ILogger<SaveContextToDatabaseStep>>().Object;
      DbProviderFactory providerFactory = new Mock<DbProviderFactory>().Object;
      var options = new SaveContextToDatabaseOptions();
      IPipelineRequest next = new Mock<IPipelineRequest>().Object;

      IPipelineContext context = null;

      var sut = new SaveContextToDatabaseStep(logger, providerFactory, options, next);

      var expectedParamName = nameof(context);
      // act
      var result = await Assert.ThrowsExceptionAsync<ArgumentNullException>(async () => await sut.InvokeAsync(context));

      // assert
      Assert.IsNotNull(result);
      Assert.AreEqual(expectedParamName, result.ParamName);
    }
  }
}
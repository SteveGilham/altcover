// <copyright file="SupplementContextWithDatabaseRecordStepTests.cs" company="Chris Trout">
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
  public class SupplementContextWithDatabaseRecordStepTests
  {
    [TestMethod]
    public void Constructs_SupplementContextWithDatabaseRecordStep_Successfully()
    {
      // arrange
      ILogger<SupplementContextWithDatabaseRecordStep> logger = new Mock<ILogger<SupplementContextWithDatabaseRecordStep>>().Object;
      DbProviderFactory providerFactory = new Mock<DbProviderFactory>().Object;
      var options = new SupplementContextWithDatabaseRecordOptions();
      IPipelineRequest next = new Mock<IPipelineRequest>().Object;

      // act
      var result = new SupplementContextWithDatabaseRecordStep(logger, providerFactory, options, next);

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
      var logger = new Mock<ILogger<SupplementContextWithDatabaseRecordStep>>().Object;

      var providerFactoryMock = new Mock<DbProviderFactory>();
      providerFactoryMock.Setup(x => x.CreateConnection()).Returns(null as DbConnection);
      var providerFactory = providerFactoryMock.Object;
      var environmentVariableTarget = RuntimeInformation.IsOSPlatform(OSPlatform.Windows) ? EnvironmentVariableTarget.Machine : EnvironmentVariableTarget.Process;

      var options = new SupplementContextWithDatabaseRecordOptions()
      {
        SqlStatement = new SqlStatement()
        {
          CommandType = System.Data.CommandType.StoredProcedure,
          ParameterNames = new List<string>() { "CartoonId" },
          Statement = "dbo.CartoonSelect"
        },
        RetrieveConnectionStringAsync = () => { return Task.FromResult(Environment.GetEnvironmentVariable("PIPELINE_TEST_AZURE_SQL_SERVER_CONNECTION_STRING", environmentVariableTarget)); }
      };

      int expectedId = -1;

      var context = new PipelineContext();
      context.Items.Add("CartoonId", expectedId);

      IPipelineRequest next = new Mock<IPipelineRequest>().Object;

      var sut = new SupplementContextWithDatabaseRecordStep(logger, providerFactory, options, next);

      int expectedErrorCount = 1;
      string expectedMessage = "expected"; // Resources.CONNECTION_IS_NULL(CultureInfo.CurrentCulture, providerFactory.GetType().Name);

      // act
      await sut.InvokeAsync(context).ConfigureAwait(false);

      // assert
      Assert.AreEqual(expectedErrorCount, context.Errors.Count);
      Assert.IsInstanceOfType(context.Errors[0], typeof(InvalidOperationException));
      Assert.AreEqual(expectedMessage, context.Errors[0].Message);
    }

    [TestMethod]
    public async Task Returns_PipelineContext_Error_From_InvokeAsync_When_Record_Is_Not_Returned()
    {
      // arrange
      var logger = new Mock<ILogger<SupplementContextWithDatabaseRecordStep>>().Object;
      var providerFactory = SqlClientFactory.Instance;
      var environmentVariableTarget = RuntimeInformation.IsOSPlatform(OSPlatform.Windows) ? EnvironmentVariableTarget.Machine : EnvironmentVariableTarget.Process;

      var options = new SupplementContextWithDatabaseRecordOptions()
      {
        SqlStatement = new SqlStatement()
        {
          CommandType = System.Data.CommandType.StoredProcedure,
          ParameterNames = new List<string>() { "CartoonId" },
          Statement = "dbo.CartoonSelect"
        },
        RetrieveConnectionStringAsync = () => { return Task.FromResult(Environment.GetEnvironmentVariable("PIPELINE_TEST_AZURE_SQL_SERVER_CONNECTION_STRING", environmentVariableTarget)); }
      };

      int expectedId = -1;

      var context = new PipelineContext();
      context.Items.Add("CartoonId", expectedId);

      var mockNext = new Mock<IPipelineRequest>();
      mockNext.Setup(x => x.InvokeAsync(It.IsAny<IPipelineContext>())).Throws(new InternalTestFailureException("next.InvokeAsync() should not have been called."));
      var next = mockNext.Object;

      var sut = new SupplementContextWithDatabaseRecordStep(logger, providerFactory, options, next);

      // act
      await sut.InvokeAsync(context).ConfigureAwait(false);

      // assert
      Assert.AreEqual(1, context.Errors.Count);
      Assert.AreEqual(/* Resources.NO_DATA_FOUND(CultureInfo.CurrentCulture, nameof(SupplementContextWithDatabaseRecordStep))*/
        "expected", context.Errors[0].Message);

      Assert.IsTrue(context.Items.ContainsKey("CartoonId"), "Context does not contain CartoonId.");
      Assert.AreEqual(expectedId, context.Items["CartoonId"]);
      Assert.IsFalse(context.Items.ContainsKey("Name"), "Context contains Name.");
      Assert.IsFalse(context.Items.ContainsKey("Description"), "Context contains Description.");
      Assert.IsFalse(context.Items.ContainsKey("IsActive"), "Context contains IsActive.");
      Assert.IsFalse(context.Items.ContainsKey("ModifiedBy"), "Context contains ModifiedBy.");
      Assert.IsFalse(context.Items.ContainsKey("ModifiedDate"), "Context contains ModifiedDate.");
    }

    [TestMethod]
    public async Task Returns_PipelineContext_Values_From_InvokeAsync_When_Record_Is_Returned()
    {
      // arrange
      var logger = new Mock<ILogger<SupplementContextWithDatabaseRecordStep>>().Object;
      var providerFactory = SqlClientFactory.Instance;
      var environmentVariableTarget = RuntimeInformation.IsOSPlatform(OSPlatform.Windows) ? EnvironmentVariableTarget.Machine : EnvironmentVariableTarget.Process;

      var options = new SupplementContextWithDatabaseRecordOptions()
      {
        SqlStatement = new SqlStatement()
        {
          CommandType = System.Data.CommandType.StoredProcedure,
          ParameterNames = new List<string>() { "CartoonId" },
          Statement = "dbo.CartoonSelect"
        },
        RetrieveConnectionStringAsync = () => { return Task.FromResult(Environment.GetEnvironmentVariable("PIPELINE_TEST_AZURE_SQL_SERVER_CONNECTION_STRING", environmentVariableTarget)); }
      };

      int expectedId = 1;
      string expectedName = "Bugs Bunny";
      string expectedDescription = "What's up, Doc?";
      bool expectedIsActive = true;

      var context = new PipelineContext();
      context.Items.Add("CartoonId", expectedId);

      var mockNext = new Mock<IPipelineRequest>();
      // Verify that these values are passed to the next step in the pipeline.
      mockNext.Setup(x => x.InvokeAsync(context)).Callback(() =>
          {
            Assert.IsTrue(context.Items.ContainsKey("CartoonId"), "Context does not contain CartoonId.");
            Assert.AreEqual(expectedId, context.Items["CartoonId"]);
            Assert.IsTrue(context.Items.ContainsKey("Name"), "Context does not contain Name.");
            Assert.AreEqual(expectedName, context.Items["Name"]);
            Assert.IsTrue(context.Items.ContainsKey("Description"), "Context does not contain Description.");
            Assert.AreEqual(expectedDescription, context.Items["Description"]);
            Assert.IsTrue(context.Items.ContainsKey("IsActive"), "Context does not contain IsActive.");
            Assert.AreEqual(expectedIsActive, context.Items["IsActive"]);
            Assert.IsTrue(context.Items.ContainsKey("ModifiedBy"), "Context contains ModifiedBy.");
            Assert.IsTrue(context.Items.ContainsKey("ModifiedDate"), "Context contains ModifiedDate.");
          });
      var next = mockNext.Object;

      using (var connection = providerFactory.CreateConnection())
      {
        connection.ConnectionString = options.RetrieveConnectionStringAsync.Invoke().Result;
        await connection.ExecuteAsync("dbo.CartoonInsert", new { CartoonId = expectedId, Name = expectedName, Description = expectedDescription }, commandType: System.Data.CommandType.StoredProcedure).ConfigureAwait(false);
      }

      var sut = new SupplementContextWithDatabaseRecordStep(logger, providerFactory, options, next);

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
        Assert.IsTrue(context.Items.ContainsKey("CartoonId"), "Context does not contain CartoonId.");
        Assert.AreEqual(expectedId, context.Items["CartoonId"]);
        Assert.IsFalse(context.Items.ContainsKey("Name"), "Context contains Name.");
        Assert.IsFalse(context.Items.ContainsKey("Description"), "Context contains Description.");
        Assert.IsFalse(context.Items.ContainsKey("IsActive"), "Context contains IsActive.");
        Assert.IsFalse(context.Items.ContainsKey("ModifiedBy"), "Context contains ModifiedBy.");
        Assert.IsFalse(context.Items.ContainsKey("ModifiedDate"), "Context contains ModifiedDate.");
      }
      finally
      {
        // cleanup
#pragma warning disable IDE0063 // Use simple 'using' statement
        using (var connection = providerFactory.CreateConnection())
#pragma warning restore IDE0063 // Use simple 'using' statement
        {
          connection.ConnectionString = options.RetrieveConnectionStringAsync.Invoke().Result;
          await connection.ExecuteAsync("DELETE FROM dbo.Cartoon WHERE CartoonId = @CartoonId;", new { CartoonId = expectedId }, commandType: System.Data.CommandType.Text).ConfigureAwait(false);
        }
      }
    }

    [TestMethod]
    public void Throw_ArgumentNullException_From_Constructor_When_Logger_Is_Null()
    {
      // arrange
      ILogger<SupplementContextWithDatabaseRecordStep> logger = null;
      DbProviderFactory providerFactory = new Mock<DbProviderFactory>().Object;
      var options = new SupplementContextWithDatabaseRecordOptions();
      IPipelineRequest next = new Mock<IPipelineRequest>().Object;

      string expectedParamName = nameof(logger);

      // act
      var result = Assert.ThrowsException<ArgumentNullException>(() => new SupplementContextWithDatabaseRecordStep(logger, providerFactory, options, next));

      // assert
      Assert.IsNotNull(result);
      Assert.AreEqual(expectedParamName, result.ParamName);
    }

    [TestMethod]
    public void Throws_ArgumentNullException_From_Constructor_When_Next_Is_Null()
    {
      // arrange
      ILogger<SupplementContextWithDatabaseRecordStep> logger = new Mock<ILogger<SupplementContextWithDatabaseRecordStep>>().Object;
      DbProviderFactory providerFactory = new Mock<DbProviderFactory>().Object;
      var options = new SupplementContextWithDatabaseRecordOptions();
      IPipelineRequest next = null;

      string expectedParamName = nameof(next);

      // act
      var result = Assert.ThrowsException<ArgumentNullException>(() => new SupplementContextWithDatabaseRecordStep(logger, providerFactory, options, next));

      // assert
      Assert.IsNotNull(result);
      Assert.AreEqual(expectedParamName, result.ParamName);
    }

    [TestMethod]
    public void Throws_ArgumentNullException_From_Constructor_When_Options_Is_Null()
    {
      // arrange
      ILogger<SupplementContextWithDatabaseRecordStep> logger = new Mock<ILogger<SupplementContextWithDatabaseRecordStep>>().Object;
      DbProviderFactory providerFactory = new Mock<DbProviderFactory>().Object;
      SupplementContextWithDatabaseRecordOptions options = null;
      IPipelineRequest next = new Mock<IPipelineRequest>().Object;

      string expectedParamName = nameof(options);

      // act
      var result = Assert.ThrowsException<ArgumentNullException>(() => new SupplementContextWithDatabaseRecordStep(logger, providerFactory, options, next));

      // assert
      Assert.IsNotNull(result);
      Assert.AreEqual(expectedParamName, result.ParamName);
    }

    [TestMethod]
    public void Throws_ArgumentNullException_From_Constructor_When_ProviderFactory_Is_Null()
    {
      // arrange
      ILogger<SupplementContextWithDatabaseRecordStep> logger = new Mock<ILogger<SupplementContextWithDatabaseRecordStep>>().Object;
      DbProviderFactory providerFactory = null;
      var options = new SupplementContextWithDatabaseRecordOptions();
      IPipelineRequest next = new Mock<IPipelineRequest>().Object;

      string expectedParamName = nameof(providerFactory);

      // act
      var result = Assert.ThrowsException<ArgumentNullException>(() => new SupplementContextWithDatabaseRecordStep(logger, providerFactory, options, next));

      // assert
      Assert.IsNotNull(result);
      Assert.AreEqual(expectedParamName, result.ParamName);
    }

    [TestMethod]
    public async Task Throw_ArgumentNullException_From_InvokeCoreAsync_When_IPipelineContext_Is_Null()
    {
      // arrange
      ILogger<SupplementContextWithDatabaseRecordStep> logger = new Mock<ILogger<SupplementContextWithDatabaseRecordStep>>().Object;
      DbProviderFactory providerFactory = new Mock<DbProviderFactory>().Object;
      var options = new SupplementContextWithDatabaseRecordOptions();
      IPipelineRequest next = new Mock<IPipelineRequest>().Object;

      IPipelineContext context = null;

      var sut = new SupplementContextWithDatabaseRecordStep(logger, providerFactory, options, next);

      var expectedParamName = nameof(context);
      // act
      var result = await Assert.ThrowsExceptionAsync<ArgumentNullException>(async () => await sut.InvokeAsync(context));

      // assert
      Assert.IsNotNull(result);
      Assert.AreEqual(expectedParamName, result.ParamName);
    }
  }
}
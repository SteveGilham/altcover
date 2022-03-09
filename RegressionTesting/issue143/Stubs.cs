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

namespace MyTrout.Pipelines.Steps.Data
{
  public class SqlStatement
  {
    public System.Data.CommandType CommandType { get; set; }
    public string Name { get; set; }

    public string Statement { get; set; }

    public List<string> ParameterNames { get; set; }

    public void AssertValueIsNotNull(object o)
    {
    }
  }

  public class SaveContextToDatabaseOptions
  {
    public List<SqlStatement> SqlStatements { get; set; }
    public Func<Task<string>> RetrieveConnectionStringAsync { get; set; }
  }

  public class SupplementContextWithDatabaseRecordOptions
  {
    public Func<Task<string>> RetrieveConnectionStringAsync { get; set; }
    public SqlStatement SqlStatement { get; set; }
  }

  public static class DatabaseConstants
  {
    public const string DATABASE_STATEMENT_NAME = "dummy";
    public const string DATABASE_ROWS_AFFECTED = "dummy";
  }
}
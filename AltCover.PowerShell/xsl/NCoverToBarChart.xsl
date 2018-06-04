<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:msxsl="urn:schemas-microsoft-com:xslt">
  <xsl:template match="coverage">
    <html lang="en">
      <head>
        <meta charset="utf-8" />
        <title>AltCover Code Coverage Report</title>
        <style>
          body {
          font: small verdana, arial, helvetica;
          color:#000000;
          }

          p {
          line-height:1.5em;
          margin-top:0.5em; margin-bottom:1.0em;
          }
          h1 {
          margin: 0px 0px 5px;
          font: bold larger arial, verdana, helvetica;
          }
          h2 {
          margin-top: 1em; margin-bottom: 0.5em;
          font: larger verdana,arial,helvetica
          }
          h3 {
          margin-bottom: 0.5em; font: bold 13px verdana,arial,helvetica
          }
          h4 {
          margin-bottom: 0.5em; font: bold 100% verdana,arial,helvetica
          }
          h5 {
          margin-bottom: 0.5em; font: bold 100% verdana,arial,helvetica
          }
          h6 {
          margin-bottom: 0.5em; font: bold 100% verdana,arial,helvetica
          }
          .notVisited { background:red; }
          .excluded { background: skyblue; }
          .visited { background: #90ee90; }
          .declared { background:orange; }
          .automatic { background:yellow; }
          .static { background:silver;}
          .title { font-size: 12px; font-weight: bold; }
          .assembly { font-size: 100%; font-weight: bold; font-size: 11px}
          .class { font-size:100%; cursor: pointer; color: #444444; font-size: 11px}
          .module { color: navy; font-size: 12px; }
          .method {cursor: pointer; color: #444444; font-size: 10px; font-weight: bold; }
          .subtitle { color: black; font-size: 10px; font-weight: bold; }
          th  {font-size:9px; background-color: #DDEEFF; }
          .datacell tbody {font-size:9px; text-align: right; }
          .datacell td {background-color: #FFFFEE;}
          .datacell td:last-child, .datacell th:last-child { width:100%; text-align: left; }
          td.hldatacell {background-color: #FFCCCC; }
          td.exdatacell {background-color: #DDEEFF; }
          .detailPercent {  font-size: 9px; font-weight: bold; padding-top: 1px; padding-bottom: 1px; padding-left: 3px; padding-right: 3px;}
        </style>
        <script language="JavaScript">
          <![CDATA[
				function toggle (field)
				{ field.style.display = (field.style.display == "block") ? "none" : "block"; }

				function SwitchAll(how)
				{	var len = document.all.length-1;
					for(i=0;i!=len;i++)	{
						var block = document.all[i];
						if (block != null && block.id != '')
						{ block.style.display=how;}
					}
				}

				function ExpandAll()
				{SwitchAll('block');}

				function CollapseAll()
				{SwitchAll('none');}
				//]]></script>
      </head>
      <body>
        <a name="#top"></a>
        <xsl:call-template name="header" />
        <h3>Legend</h3>
        <table width="30%">
          <tbody>
            <tr>
              <td width="50%">Uncovered code</td>
              <td width="50%" class="notVisited"></td>
            </tr>
            <tr>
              <td width="50%">User exempted</td>
              <td width="50%" class="declared"></td>
            </tr>
            <tr>
              <td width="50%">Generated code</td>
              <td width="50%" class="automatic"></td>
            </tr>
            <tr>
              <td width="50%">Statically analyzed</td>
              <td width="50%" class="static"></td>
            </tr>
            <tr>
              <td width="50%">Covered code</td>
              <td width="50%" class="visited"></td>
            </tr>
          </tbody>
        </table>

        <hr />

        <xsl:call-template name="ModuleSummary" />
        <xsl:call-template name="module" />
        <hr />
        <xsl:call-template name="justifications" />
        <xsl:call-template name="footer" />
        <script language="JavaScript">
          CollapseAll();//</script>
      </body>
    </html>
  </xsl:template>
  <xsl:template name="module">
    <xsl:for-each select="//module">
      <xsl:sort select="@assembly" />
      <xsl:variable name="module" select="./@assembly" />
      <div class="assembly">
        <a name="#{generate-id($module)}">
          Module
          <xsl:value-of select="$module" />
        </a>
      </div>
      <xsl:for-each select="./method[not(./@class = preceding-sibling::method/@class)]">
        <xsl:sort select="@class" />
        <xsl:sort select="@name" />
        <xsl:call-template name="ClassSummary">
          <xsl:with-param name="module" select="$module" />
          <xsl:with-param name="class" select="./@class" />
        </xsl:call-template>
      </xsl:for-each>
    </xsl:for-each>
    <xsl:variable name="totalMod" select="count(./method/seqpnt[@excluded='false'])" />
    <xsl:variable name="notvisitedMod" select="count(./method/seqpnt[ @visitcount='0'][@excluded='false'] ) div $totalMod * 100 " />
    <xsl:variable name="rawVisitedMod" select="count(./method/seqpnt[not(@visitcount='0')] )" />

    <xsl:variable name="rawDeclaredMod" select="count(./method/seqpnt[(@visitcount='-1')] )" />
    <xsl:variable name="rawAutoMod" select="count(./method/seqpnt[(@visitcount='-2')] )" />
    <xsl:variable name="rawStaticMod" select="count(./method/seqpnt[(@visitcount='-3')] )" />
    <xsl:variable name="actualVisitedMod" select="$rawVisitedMod - ($rawDeclaredMod + $rawAutoMod + $rawStaticMod)" />
    <xsl:variable name="visitedMod" select="$actualVisitedMod div $totalMod * 100" />
    <xsl:variable name="declaredMod" select="$rawDeclaredMod div $totalMod * 100" />
    <xsl:variable name="autoMod" select="$rawAutoMod div $totalMod * 100" />
    <xsl:variable name="staticMod" select="$rawStaticMod div $totalMod * 100" />
  </xsl:template>
  <xsl:template name="Methods">
    <xsl:param name="module" />
    <xsl:param name="class" />
    <xsl:for-each select="//method[(@class = $class) and (parent::module/@assembly=$module)]">
      <xsl:sort select="@name" />
      <xsl:variable name="total" select="count(./seqpnt[@excluded='false'])" />
      <xsl:variable name="notvisited" select="count(./seqpnt[@visitcount='0'][@excluded='false'] ) " />
      <xsl:variable name="rawvisited" select="count(./seqpnt[not(@visitcount='0')])" />
      <xsl:variable name="declared" select="count(./seqpnt[(@visitcount='-1')] )" />
      <xsl:variable name="auto" select="count(./seqpnt[(@visitcount='-2')] )" />
      <xsl:variable name="static" select="count(./seqpnt[(@visitcount='-3')] )" />
      <xsl:variable name="visited" select="$rawvisited - ($declared + $auto + $static)" />
      <xsl:variable name="methid" select="generate-id(.)" />
      <table cellpadding="3" cellspacing="0" width="90%">
        <tr>
          <td width="45%" class='method'>
            <xsl:attribute name="onclick">javascript:toggle(<xsl:value-of select="$methid" />)</xsl:attribute>
            <xsl:value-of select="@name" />
          </td>
          <td width="55%">
            <xsl:call-template name="detailPercent">
              <xsl:with-param name="visited" select="$visited" />
              <xsl:with-param name="declared" select="$declared" />
              <xsl:with-param name="auto" select="$auto" />
              <xsl:with-param name="static" select="$static" />
              <xsl:with-param name="notVisited" select="$notvisited" />
              <xsl:with-param name="total" select="$total" />
            </xsl:call-template>
          </td>
        </tr>
      </table>
      <xsl:call-template name="seqpnt">
        <xsl:with-param name="module" select="$module" />
        <xsl:with-param name="class" select="$class" />
        <xsl:with-param name="id" select="$methid" />
      </xsl:call-template>
    </xsl:for-each>
  </xsl:template>
  <xsl:template name="seqpnt">
    <xsl:param name="module" />
    <xsl:param name="class" />
    <xsl:param name="id" />
    <table class="datacell" cellpadding="3" cellspacing="0" border='1' width="90%" bordercolor="black" style="display: block;">
      <xsl:attribute name="id">
        <xsl:value-of select="$id" />
      </xsl:attribute>
      <thead>
        <tr>
          <th>Visits</th>
          <th>Line</th>
          <th>End</th>
          <th>Column</th>
          <th>End</th>
          <th>Document</th>
        </tr>
      </thead>
      <tbody>
      <xsl:for-each select="./seqpnt">
        <xsl:sort select="@line" />
        <tr>
          <td>
            <xsl:attribute name="class">
              <xsl:choose>
                <xsl:when test="@excluded = 'true'">exdatacell</xsl:when>
                <xsl:when test="@visitcount = 0">hldatacell</xsl:when>
                <xsl:otherwise>datacell</xsl:otherwise>
              </xsl:choose>
            </xsl:attribute>
            <xsl:choose>
              <xsl:when test="@excluded = 'true'">---</xsl:when>
              <xsl:otherwise>
                <xsl:value-of select="@visitcount" />
              </xsl:otherwise>
            </xsl:choose>
          </td>
          <td>
            <xsl:value-of select="@line" />
          </td>
          <td>
            <xsl:value-of select="@endline" />
          </td>
          <td>
            <xsl:value-of select="@column" />
          </td>
          <td>
            <xsl:value-of select="@endcolumn" />
          </td>
          <td>
            <xsl:value-of select="@document" />
          </td>
        </tr>
      </xsl:for-each>
      </tbody>        
    </table>
  </xsl:template>
  <!-- Class Summary -->
  <xsl:template name="ClassSummary">
    <xsl:param name="module" />
    <xsl:param name="class" />
    <xsl:variable name="total" select="count(//seqpnt[(parent::method/parent::module/@assembly=$module) and (parent::method/@class=$class) and (@excluded='false') ])" />
    <xsl:variable name="notvisited" select="count(//seqpnt[(parent::method/parent::module/@assembly=$module)and (parent::method/@class=$class) and (@visitcount='0') and (@excluded='false')] )" />

    <xsl:variable name="rawvisited" select="count(//seqpnt[(parent::method/parent::module/@assembly=$module) and (parent::method/@class=$class) and (not(@visitcount='0'))] )" />
    <xsl:variable name="declared" select="count(//seqpnt[(parent::method/parent::module/@assembly=$module)and (parent::method/@class=$class) and (@visitcount='-1') and (@excluded='false')] )" />
    <xsl:variable name="auto" select="count(//seqpnt[(parent::method/parent::module/@assembly=$module)and (parent::method/@class=$class) and (@visitcount='-2') and (@excluded='false')] )" />
    <xsl:variable name="static" select="count(//seqpnt[(parent::method/parent::module/@assembly=$module)and (parent::method/@class=$class) and (@visitcount='-3') and (@excluded='false')] )" />

    <xsl:variable name="visited" select="$rawvisited - ($declared + $auto + $static)" />

    <xsl:variable name="newid" select="concat (generate-id(), 'class')" />
    <table width='90%'>
      <tr>
        <td width="40%" class="class">
          <xsl:attribute name="onclick">javascript:toggle(<xsl:value-of select="$newid" />)</xsl:attribute>
          <xsl:value-of select="$class" />
        </td>
        <td width="60%">
          <xsl:call-template name="detailPercent">
            <xsl:with-param name="visited" select="$visited" />
            <xsl:with-param name="declared" select="$declared" />
            <xsl:with-param name="auto" select="$auto" />
            <xsl:with-param name="static" select="$static" />
            <xsl:with-param name="notVisited" select="$notvisited" />
            <xsl:with-param name="total" select="$total" />
          </xsl:call-template>
        </td>
      </tr>
      <tr>
        <div style="display: block;" width="100%">
            <div>
              <xsl:attribute name="id">
                <xsl:value-of select="$newid" />
              </xsl:attribute>
              <xsl:call-template name="Methods">
                <xsl:with-param name="module" select="$module" />
                <xsl:with-param name="class" select="$class" />
              </xsl:call-template>
            </div>
        </div>
      </tr>
    </table>
    <hr size="1" width='90%' align='left' style=" border-bottom: 1px dotted #999;" />
  </xsl:template>
  <xsl:template name="ClassSummaryDetail">
    <xsl:param name="module" />
    <xsl:variable name="total" select="count(./method/seqpnt[ @excluded='false' ])" />
    <xsl:variable name="notVisited" select="count( ./method/seqpnt[ @visitcount='0'][ @excluded='false' ] )" />
    <xsl:variable name="rawvisited" select="count(./method/seqpnt[not(@visitcount='0')])" />
    <xsl:variable name="declared" select="count(./method/seqpnt[(@visitcount='-1')] )" />
    <xsl:variable name="auto" select="count(./method/seqpnt[(@visitcount='-2')] )" />
    <xsl:variable name="static" select="count(./method/seqpnt[(@visitcount='-3')] )" />
    <xsl:variable name="visited" select="count(./method/seqpnt[not(@visitcount='0')] )" />
    <td width="35%">
      <div class="assembly">
        <a href="#{generate-id($module)}">
          <xsl:value-of select="$module" />
        </a>
      </div>
    </td>
    <td width="65%">
      <xsl:call-template name="detailPercent">
        <xsl:with-param name="visited" select="$visited" />
        <xsl:with-param name="declared" select="$declared" />
        <xsl:with-param name="auto" select="$auto" />
        <xsl:with-param name="static" select="$static" />
        <xsl:with-param name="notVisited" select="$notVisited" />
        <xsl:with-param name="total" select="$total" />
      </xsl:call-template>
    </td>
  </xsl:template>
  <!-- Modules Summary -->
  <xsl:template name="ModuleSummary">
    <H2>Modules summary</H2>
    <xsl:for-each select="//module">
      <xsl:sort select="@assembly" />
      <table width='90%'>
        <tr>
          <xsl:call-template name="ModuleSummaryDetail">
            <xsl:with-param name="module" select="./@assembly" />
          </xsl:call-template>
        </tr>
      </table>
    </xsl:for-each>
    <hr size="1" />
  </xsl:template>
  <xsl:template name="ModuleSummaryDetail">
    <xsl:param name="module" />
    <xsl:variable name="total" select="count(./method/seqpnt[@excluded='false'])" />
    <xsl:variable name="notVisited" select="count( ./method/seqpnt[ @visitcount='0' ][ @excluded='false' ] )" />
    <xsl:variable name="rawvisited" select="count(./method/seqpnt[not(@visitcount='0')])" />
    <xsl:variable name="declared" select="count(./method/seqpnt[(@visitcount='-1')] )" />
    <xsl:variable name="auto" select="count(./method/seqpnt[(@visitcount='-2')] )" />
    <xsl:variable name="static" select="count(./method/seqpnt[(@visitcount='-3')] )" />
    <xsl:variable name="visited" select="$rawvisited - ($declared + $auto + $static)" />
    <td width="30%">
      <div class="assembly">
        <a href="#{generate-id($module)}">
          <xsl:value-of select="$module" />
        </a>
      </div>
    </td>
    <td width="70%">
      <xsl:call-template name="detailPercent">
        <xsl:with-param name="visited" select="$visited" />
        <xsl:with-param name="declared" select="$declared" />
        <xsl:with-param name="auto" select="$auto" />
        <xsl:with-param name="static" select="$static" />
        <xsl:with-param name="notVisited" select="$notVisited" />
        <xsl:with-param name="total" select="$total" />
      </xsl:call-template>
    </td>
  </xsl:template>
  <!-- General Header -->
  <xsl:template name="header">
    <h1>
      <b>AltCover</b> Code Coverage Report
    </h1>
    <table>
      <tr>
        <td class="class">
          <a onClick="ExpandAll();">Expand</a>
        </td>
        <td> | </td>
        <td class="class">
          <a onClick="CollapseAll();">Collapse</a>
        </td>
      </tr>
    </table>
    <hr size="1" />
  </xsl:template>
  <xsl:template name="footer">
    <hr size="1" />
    <a class="detailPercent" href="#{top}">Top</a>
  </xsl:template>
  <!-- draw % table-->
  <xsl:template name="detailPercent">
    <xsl:param name="visited" />
    <xsl:param name="declared" />
    <xsl:param name="auto" />
    <xsl:param name="static" />
    <xsl:param name="notVisited" />
    <xsl:param name="total" />
    <table width="100%" class="detailPercent">
      <tr>
        <xsl:if test="($notVisited=0) and ($visited=0) and (($declared + $auto + $static)=0)">
          <td class="excluded" width="100%">Excluded</td>
        </xsl:if>
        <xsl:if test="not($notVisited=0)">
          <td class="notVisited">
            <xsl:attribute name="width">
              <xsl:value-of select="concat($notVisited div $total * 100,'%')" />
            </xsl:attribute>
            <xsl:value-of select="concat (format-number($notVisited div $total * 100,'#.##'),'%')" />
          </td>
        </xsl:if>
        <xsl:if test="not ($declared=0)">
          <td class="declared">
            <xsl:attribute name="width">
              <xsl:value-of select="concat($declared div $total * 100,'%')" />
            </xsl:attribute>
            <xsl:value-of select="concat (format-number($declared div $total * 100,'#.##'), '%')" />
          </td>
        </xsl:if>
        <xsl:if test="not ($auto=0)">
          <td class="automatic">
            <xsl:attribute name="width">
              <xsl:value-of select="concat($auto div $total * 100,'%')" />
            </xsl:attribute>
            <xsl:value-of select="concat (format-number($auto div $total * 100,'#.##'), '%')" />
          </td>
        </xsl:if>
        <xsl:if test="not ($static=0)">
          <td class="static">
            <xsl:attribute name="width">
              <xsl:value-of select="concat($static div $total * 100,'%')" />
            </xsl:attribute>
            <xsl:value-of select="concat (format-number($static div $total * 100,'#.##'), '%')" />
          </td>
        </xsl:if>
        <xsl:if test="not ($visited=0)">
          <td class="visited">
            <xsl:attribute name="width">
              <xsl:value-of select="concat($visited div $total * 100,'%')" />
            </xsl:attribute>
            <xsl:value-of select="concat (format-number($visited div $total * 100,'#.##'), '%')" />
          </td>
        </xsl:if>
      </tr>
    </table>
  </xsl:template>

  <xsl:key name="justification" match="method" use="@excluded-because" />

  <xsl:template name="justifications">
    <h2>
      <a name="justifications">Coverage exemptions in the code</a>
    </h2>
    <dl>
      <xsl:for-each select="//method[@excluded-because][generate-id(.)=generate-id(key('justification',@excluded-because)[1])]">
        <xsl:sort select="@excluded-because" />
        <dt>
          <xsl:value-of select="@excluded-because" />
        </dt>
        <xsl:for-each select="key('justification',@excluded-because)">
          <xsl:sort select="@class" />
          <xsl:sort select="@name" />
          <dd>
            <xsl:value-of select="@class" />
            <xsl:text>.</xsl:text>
            <xsl:value-of select="@name" />
          </dd>
        </xsl:for-each>
      </xsl:for-each>
    </dl>
  </xsl:template>
</xsl:stylesheet>
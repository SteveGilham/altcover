<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:msxsl="urn:schemas-microsoft-com:xslt" xmlns:user="urn:my-scripts">

  <xsl:output method="xml" />

  <xsl:template match="coverage">
    <coverage profilerVersion="Cobertura" driverVersion="Cobertura" startTime="now" measureTime="now" lineonly="true">

      <xsl:for-each select="//package">
        <xsl:variable name="module" select="@name" />
        <xsl:variable name="moduleName" select="@name" />
        <module moduleId="{@name}">
          <xsl:attribute name="name">
            <xsl:value-of select="$module" />
          </xsl:attribute>
          <xsl:attribute name="assembly">
            <xsl:value-of select="$moduleName" />
          </xsl:attribute>
          <xsl:attribute name="assemblyIdentity">
            <xsl:value-of select="$moduleName" />
          </xsl:attribute>
          <xsl:for-each select="descendant::method">
            <xsl:variable name="class" select="../../@name" />
            <xsl:variable name="file" select="../../@filename" />
            <xsl:variable name="method" select="@name" />

            <method excluded="false" instrumented="true">
              <xsl:attribute name="name">
                <xsl:value-of select="$method" />
              </xsl:attribute>
              <xsl:attribute name="class">
                <xsl:value-of select="$class" />
              </xsl:attribute>
              <xsl:attribute name="fullname">
                <xsl:value-of select="concat($class, '::', $method, '(...)')" />
              </xsl:attribute>
              <xsl:attribute name="document">
                <xsl:value-of select="$file" />
              </xsl:attribute>

              <xsl:for-each select="descendant::line">
                <xsl:sort select="@line" data-type="number" />
                    <seqpnt visitcount="{@hits}" line="{@number}" column="1" endline="{@number}" endcolumn="2" offset="{@number}" excluded="false">
                      <xsl:attribute name="document">
                        <xsl:value-of select="$file" />
                      </xsl:attribute>
                    </seqpnt>
              </xsl:for-each>
            </method>
          </xsl:for-each>
        </module>
      </xsl:for-each>
    </coverage>
  </xsl:template>
</xsl:stylesheet>
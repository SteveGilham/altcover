<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:msxsl="urn:schemas-microsoft-com:xslt" xmlns:user="urn:my-scripts">

  <xsl:output method="xml" />

  <xsl:template match="CoverageSession">
    <coverage profilerVersion="OpenCover" driverVersion="OpenCover" startTime="now" measureTime="now">

      <xsl:for-each select="//Module[not(@skippedDueTo)]">
        <xsl:variable name="module" select="./ModulePath" />
        <xsl:variable name="moduleName" select="./ModuleName" />
        <module moduleId="{@hash}">
          <xsl:attribute name="name">
            <xsl:value-of select="$module" />
          </xsl:attribute>
          <xsl:attribute name="assembly">
            <xsl:value-of select="$moduleName" />
          </xsl:attribute>
          <xsl:attribute name="assemblyIdentity">
            <xsl:value-of select="$moduleName" />
          </xsl:attribute>
          <xsl:for-each select="descendant::Method[not(@skippedDueTo)]">
            <xsl:variable name="class" select="../../FullName" />
            <xsl:variable name="fileRef" select="./FileRef/@uid" />
            <xsl:variable name="file" select="//File[@uid = $fileRef]/@fullPath" />
            <xsl:variable name="method" select="./Name" />

            <method excluded="false" instrumented="true">
              <xsl:attribute name="name">
                <xsl:value-of select="$method" />
              </xsl:attribute>
              <xsl:attribute name="class">
                <xsl:value-of select="$class" />
              </xsl:attribute>
              <xsl:attribute name="fullname">
                <xsl:value-of select="$method" />
              </xsl:attribute>

              <xsl:for-each select="descendant::SequencePoint">
                <seqpnt visitcount="{@vc}" line="{@sl}" column="{@sc}" endline="{@el}" endcolumn="{@ec}" offset="{@offset}" excluded="false">
                  <xsl:attribute name="document">
                    <xsl:value-of select="$file" />
                  </xsl:attribute>
                </seqpnt>
              </xsl:for-each>
            </method>
          </xsl:for-each>
          <xsl:for-each select="./Files/File[@altcover.embed]">
            <altcover.file document="{@fullPath}" embed="{@altcover.embed}" />
          </xsl:for-each>
        </module>
      </xsl:for-each>
    </coverage>
  </xsl:template>
</xsl:stylesheet>
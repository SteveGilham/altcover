<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:msxsl="urn:schemas-microsoft-com:xslt" xmlns:user="urn:my-scripts">

  <msxsl:script language="C#" implements-prefix="user">
    <![CDATA[
     public string filename(string path){
       return (new System.IO.FileInfo(path)).Name;
     }
      ]]>
  </msxsl:script>
  <msxsl:script language="C#" implements-prefix="user">
    <![CDATA[
     public string identity(string path, string fallback){
       if (System.IO.File.Exists(path))
         try { return System.Reflection.Assembly.LoadFile(path).FullName; }  catch (Exception) { }
       return fallback;
     }
      ]]>
  </msxsl:script>
    <msxsl:script language="C#" implements-prefix="user">
    <![CDATA[
     public string now(){
       return System.DateTime.UtcNow.ToLongDateString() + ":" + System.DateTime.UtcNow.ToLongTimeString();
     }
      ]]>
  </msxsl:script>
  <msxsl:script language="C#" implements-prefix="user">
    <![CDATA[
     public string methodname(string name){
       string lead = name.Substring(name.LastIndexOf("::") + 2);
       return lead.Substring(0, lead.IndexOf("("));
     }
      ]]>
  </msxsl:script>
  <msxsl:script language="C#" implements-prefix="user">
    <![CDATA[
     public string classname(string name){
       return name.Replace("/", "+");
     }
      ]]>
  </msxsl:script>

  <xsl:output method="xml" />

  <xsl:template match="CoverageSession">
    <coverage profilerVersion="OpenCover" driverVersion="OpenCover">
        <xsl:attribute name="startTime">
          <xsl:value-of select="user:now()" />
        </xsl:attribute>
        <xsl:attribute name="measureTime">
          <xsl:value-of select="user:now()" />
        </xsl:attribute>

      <xsl:for-each select="//Module[not(@skippedDueTo)]">
        <xsl:variable name="module" select="./ModulePath" />
        <xsl:variable name="moduleName" select="./ModuleName" />
        <module moduleId="{@hash}">
          <xsl:attribute name="name">
            <xsl:value-of select="user:filename($module)" />
          </xsl:attribute>
          <xsl:attribute name="assembly">
            <xsl:value-of select="$moduleName" />
          </xsl:attribute>
          <xsl:attribute name="assemblyIdentity">
            <xsl:value-of select="user:identity($module, $moduleName)" />
          </xsl:attribute>
            <xsl:for-each select="descendant::Method">
              <xsl:variable name="class" select="../../FullName" />
              <xsl:variable name="token" select="./MetadataToken" />
              <xsl:variable name="fileRef" select="./FileRef/@uid" />
              <xsl:variable name="file" select="//File[@uid = $fileRef]/@fullPath" />
              <xsl:variable name="method" select="./Name" />

              <method excluded="false" instrumented="true">
                  <xsl:attribute name="name">
                    <xsl:value-of select="user:methodname($method)" />
                  </xsl:attribute>
                  <xsl:attribute name="class">
                    <xsl:value-of select="user:classname($class)" />
                  </xsl:attribute>
                  <xsl:attribute name="metadataToken">
                    <xsl:value-of select="$token" />
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
		
                  <xsl:for-each select="descendant::BranchPoint">
                    <branch visitcount="{@vc}" path="{@path}" offset="{@offset}" excluded="false">
                    </branch>
		</xsl:for-each>
		
              </method>
            </xsl:for-each>
        </module>
      </xsl:for-each>
    </coverage>
  </xsl:template>
</xsl:stylesheet>
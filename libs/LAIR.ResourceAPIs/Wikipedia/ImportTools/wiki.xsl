<?xml version="1.0" encoding="utf8"?>

<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:preserve-space elements="text" />
  <xsl:output type="text" encoding="utf-8"/>

  <xsl:template match="/">
    <xsl:for-each select="mediawiki/page">
      <xsl:text disable-output-escaping="yes">insert into page (page_namespace, page_title, page_restrictions, page_random, page_latest, page_len) values (0,"</xsl:text><xsl:value-of select="translate(title, '&quot;', '&quot;&quot;')" /><xsl:text>",0,0,0,0);&#xA;</xsl:text>
    </xsl:for-each>
  </xsl:template>  

</xsl:stylesheet>

<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:xs="http://www.w3.org/2001/XMLSchema"
    exclude-result-prefixes="xs"
    version="2.0">
    
    <xsl:template match="/">
        <xsl:apply-templates select="//text/body/div1"/>
    </xsl:template>
    
    <xsl:template match="//div1">
        <xsl:apply-templates select="div2"/>
    </xsl:template>
    
    <xsl:template match="div2">
        <xsl:apply-templates select="l"/>
    </xsl:template>
    
    <xsl:template match="l">
        <xsl:value-of select="./ancestor::div1/@n"/>.<xsl:value-of select="./parent::div2/@n"/>.<xsl:number count="l" level="single"/>:<xsl:apply-templates/>
        <xsl:text>
            
        </xsl:text>
    </xsl:template>
    
    <!--
    <xsl:template match="del">
        [ <xsl:apply-templates select="."/> ]
    </xsl:template>
    -->
    <xsl:template match="@*|node()" priority="-1">
        <xsl:copy>
            <xsl:apply-templates select="@*|node()"/>
        </xsl:copy>
    </xsl:template>
    
</xsl:stylesheet>
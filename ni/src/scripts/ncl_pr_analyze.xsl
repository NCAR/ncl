<?xml version="1.0"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:template match="/">
<xsl:variable name="white_color" select="'#ffffff'"/>
<xsl:variable name="blue_color" select="'#3366ff'"/>
<xsl:variable name="green_color" select="'#99cc33'"/>
<xsl:variable name="yellow_color" select="'#ffff00'"/>
<xsl:variable name="orange_color" select="'#ff6600'"/>
<xsl:variable name="red_color" select="'#ff0000'"/>
<html>
<body>
	<h1><u> NCL Line Statistics </u></h1>
	<h3><u>	<xsl:value-of select="ncl_stats/@file"/> </u> </h3>
	<table>
		<xsl:for-each select="ncl_stats/line_stat">
			<xsl:variable name="color">
				<xsl:choose>
					<xsl:when test="line/@color = 'white'">
						<xsl:value-of select="$white_color"/>
					</xsl:when>
					<xsl:when test="line/@color = 'blue'">
						<xsl:value-of select="$blue_color"/>
					</xsl:when>
					<xsl:when test="line/@color = 'green'">
						<xsl:value-of select="$green_color"/>
					</xsl:when>
					<xsl:when test="line/@color = 'yellow'">
						<xsl:value-of select="$yellow_color"/>
					</xsl:when>
					<xsl:when test="line/@color = 'orange'">
						<xsl:value-of select="$orange_color"/>
					</xsl:when>
					<xsl:when test="line/@color = 'red'">
						<xsl:value-of select="$red_color"/>
					</xsl:when>
					<xsl:otherwise>
						<xsl:value-of select="$white_color"/>
					</xsl:otherwise> 
				</xsl:choose>	
			</xsl:variable>
			<tr bgcolor="{$color}">
				<td> <xsl:value-of select="line/@line_no"/> </td>
				<td> <xsl:value-of select="line/@time"/> </td>
				<td> <xsl:value-of select="line/@perc_time"/> </td>
				<td> <xsl:value-of select="line"/> </td>
			</tr>
		</xsl:for-each>
	</table>
</body>
</html>
</xsl:template>

</xsl:stylesheet>

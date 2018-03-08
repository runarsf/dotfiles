"Resource/UI/CharInfoPanel.res"
{
	"character_info"
	{
		"ControlName"	"EditablePanel"
		"fieldName"		"character_info"
		"xpos"			"0"
		"ypos"			"0"
		"wide"			"f0"
		"tall"			"480"
		"autoResize"	"0"
		"pinCorner"		"0"
		"visible"		"1"
		"enabled"		"1"
		"tabPosition"	"0"
		"settitlebarvisible"	"1" [$WINDOWS]
		"settitlebarvisible"	"1" [!$WINDOWS]
		"PaintBackgroundType"	"0"
		"bgcolor_override"				"46 43 42 0" [$WINDOWS]
		"infocus_bgcolor_override"		"46 43 42 0" [$WINDOWS]
		"outoffocus_bgcolor_override"	"46 43 42 0" [$WINDOWS]
		"bgcolor_override"				"46 43 42 255" [!$WINDOWS]
		"infocus_bgcolor_override"		"46 43 42 255" [!$WINDOWS]
		"outoffocus_bgcolor_override"	"46 43 42 255" [!$WINDOWS]
		
		"title"			""
		"title_font"	"G_FontNone"
		"titletextinsetX"	"40"
		"titletextinsetY"	"0"
		"titlebarfgcolor_override"				"200 187 161 255"
		"titlebardisabledfgcolor_override"		"200 187 161 255"
		"titlebarbgcolor_override"				"46 43 42 255"
		
		"clientinsetx_override"			"0"
		"sheetinset_bottom"				"0"
	}
	
	"BackgroundHeader"
	{
		"ControlName"	"EditablePanel"
		"fieldName"		"BackgroundHeader"
		"xpos"			"0"
		"ypos"			"0"
		"zpos"			"-2"
		"wide"			"f0"
		"tall"			"44"
		"visible"		"1"
		"enabled"		"1"
		"paintbackground"	"1"
		"bgcolor_override"	"G_Footer"
		"paintborder"	"1"
		"border"		"G_MenuHeader"
	}
	
	"BackgroundFooter"
	{
		"ControlName"	"EditablePanel"
		"fieldName"		"BackgroundFooter"
		"xpos"			"0"
		"ypos"			"r46"
		"zpos"			"1"
		"wide"			"f0"
		"tall"			"46"
		"visible"		"1"
		"enabled"		"1"
		"paintbackground"	"1"
		"bgcolor_override"	"G_Footer"
	}	
	
	"FooterLine"
	{
		"ControlName"	"EditablePanel"
		"fieldName"		"FooterLine"
		"xpos"			"0"
		"ypos"			"r46"
		"zpos"			"2"
		"wide"			"f0"
		"tall"			"2"
		"visible"		"1"
		"enabled"		"1"
		"paintbackground"	"1"
		"bgcolor_override"	"G_Highlight"
	}			
	
	"Sheet"
	{
		"ControlName"	"EditablePanel"
		"fieldName"		"Sheet"
		"tabxindent"	"50"
		"tabxdelta"		"10"
		"tabwidth"		"240"
		"tabheight"		"30"
		"yoffset"		"14"
		"transition_time" 	"0"
		"paintbackground"	"1"
		"bgcolor_override"	"G_Footer"
		
		"HeaderLine"
		{
			"ControlName"	"EditablePanel"
			"fieldName"		"HeaderLine"
			"xpos"			"0"
			"ypos"			"43"
			"zpos"			"2"
			"wide"			"f0"
			"tall"			"2"
			"visible"		"1"
			"enabled"		"1"
			"paintbackground"	"1"
			"bgcolor_override"	"G_Highlight"
		}
		
		"tabskv"
		{ 
			"textinsetx"		"5"
			"font"				"G_FontMedium"
			"selectedcolor"		"G_FooterText"
			"unselectedcolor"	"G_DimmFooterText"
			"defaultBgColor_override"	"0 0 0 0"
			"selectedBgColor_override"	"0 0 0 0"
			"activeBgColor_override"	"0 0 0 0"
			"paintbackground"	"0"
			"paintborder"		"0"
			"activeborder_override"	""
			"normalborder_override" ""
		}
	}
	
	"BackButton"
	{
		"ControlName"	"CExButton"
		"fieldName"		"BackButton"
		"xpos"			"c-288"
		"ypos"			"r35"
		"zpos"			"2"
		"wide"			"135"
		"tall"			"25"
		"autoResize"	"0"
		"pinCorner"		"3"
		"visible"		"1"
		"enabled"		"1"
		"tabPosition"	"0"
		"labelText"		"#TF_BackCarat"
		"font"			"HudFontSmallBold"
		"textAlignment"	"center"
		"dulltext"		"0"
		"brighttext"	"0"
		"default"		"0"
		"Command"		"back"
		"sound_depressed"	"UI/buttonclick.wav"
		"sound_released"	"UI/buttonclickrelease.wav"
	}

	"BackButtonQKey"
	{
		"ControlName"	"CExButton"
		"fieldName"		"BackButtonQKey"
		"xpos"			"-9999"
		"ypos"			"-9999"
		"zpos"			"-9999"
		"wide"			"0"
		"tall"			"0"
		"autoResize"	"0"
		"pinCorner"		"3"
		"visible"		"1"
		"enabled"		"1"
		"tabPosition"	"0"
		"labelText"		"&Q"
		"font"			"HudFontSmallBold"
		"textAlignment"	"center"
		"dulltext"		"0"
		"brighttext"	"0"
		"default"		"0"
		"Command"		"back"
	}	
	
	"NotificationsPresentPanel"
	{
		"ControlName"	"CNotificationsPresentPanel"
		"fieldName"		"NotificationsPresentPanel"
		"xpos"			"r200"
		"ypos"			"10"
		"zpos"			"10000"
		"wide"			"190"
		"tall"			"50"
		"visible"		"0"
		"enabled"		"1"
	}
}

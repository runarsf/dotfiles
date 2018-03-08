"Resource/UI/HudDemomanPipes.res"
{
	"background"
	{
		"ControlName"	"CTFImagePanel"
		"fieldName"		"background"
		"xpos"			"0"
		"ypos"			"0"
		"zpos"			"0"
		"wide"			"0"
		"tall"			"0"
		"visible"		"0"
		"enabled"		"0"
		"image"			"../hud/misc_ammo_area_blue"			
	}
	
	"ChargeLabel"
	{
		"ControlName"			"CExLabel"
		"fieldName"				"ChargeLabel"
		"xpos"					"-9999"	// 2_ CDCM
		"xpos_minmode"			"-9999"	// 3_ CDCM
		"ypos"					"c+15"	// 3 CDCM
		"ypos_minmode"			"c+15"	// 4 CDCM
		"zpos"					"10"
		"wide"					"30"	// 5 CDCM
		"wide_minmode"			"30"	// 6 CDCM
		"tall"					"4"		// 7 CDCM	
		"tall_minmode"			"4"		// 8 CDCM
		"autoResize"			"1"
		"pinCorner"				"2"
		"visible"				"1"
		"enabled"				"1"
		"tabPosition"			"0"
		"labelText"				"#TF_CHARGE"
		"textAlignment"			"west" // 1_ CDCM
		"dulltext"				"0"
		"brighttext"			"0"
		"font"					"DefaultVerySmall"
		"fgcolor_override"		"155 155 155 255"
		"textinsety"			"-1"
	}

	"ChargeMeter"
	{	
		"ControlName"			"ContinuousProgressBar"
		"fieldName"				"ChargeMeter"
		"font"					"Default"
		"xpos"					"c-15"	// 1 CDCM
		"xpos_minmode"			"c-15"	// 2 CDCM
		"ypos"					"c+15"	// 3 CDCM
		"ypos_minmode"			"c+15"	// 4 CDCM
		"zpos"					"2"
		"wide"					"30"	// 5 CDCM
		"wide_minmode"			"30"	// 6 CDCM
		"tall"					"4"		// 7 CDCM		
		"tall_minmode"			"4"		// 8 CDCM		
		"autoResize"			"0"
		"pinCorner"				"0"
		"visible"				"1"
		"enabled"				"1"
		"textAlignment"			"Left"
		"dulltext"				"0"
		"brighttext"			"0"
		"paintborder"			"1"
		"border"				"G_ChargeBorder"	// 9 CDCM
		"paintbackground"		"0"	// 10 CDCM
		"bgcolor_override"		"G_ItemMeterBg"
	}
		
	"PipesPresentPanel"
	{
		"ControlName"	"EditablePanel"
		"fieldName"		"PipesPresentPanel"
		"xpos"			"c100" // 1 CSBC
		"xpos_minmode"	"c80" // 2 CSBC
		"ypos"			"r32"
		"ypos_minmode"	"c124" // 1 BPM
		"zpos"			"1"
		"wide"			"180"
		"tall"			"126"
		"visible"		"0"
		
		"PipeIcon"
		{
			"ControlName"	"CTFImagePanel"
			"fieldName"		"PipeIcon"
			"xpos"			"4"
			"ypos"			"4"
			"ypos_minmode"	"4"
			"wide"			"0"
			"wide_minmode"	"0"
			"tall"			"0"
			"tall_minmode"	"0"
			"zpos"			"1"
			"visible"		"0"
			"enabled"		"0"
			"image"			"../hud/ico_stickybomb_red"
			"scaleImage"	"1"
			"teambg_2"		"../hud/ico_stickybomb_red"
			"teambg_3"		"../hud/ico_stickybomb_blue"	
		}	
		
		"G_PipeIcon"
		{
			"ControlName"			"CExLabel"
			"fieldName"				"G_PipeIcon"
			"xpos"					"125"
			"ypos"					"0"
			"zpos"					"2"
			"wide"					"16"
			"tall"					"25"
			"textinsetx"			"1"
			"textinsety"			"0"
			"visible"				"0" // 10 CSBC
			"enabled"				"1"
			"labelText"				"G"
			"textAlignment"			"west"
			"font"					"G_HeadsIcon"
			"fgcolor"				"G_StickyIconColor"
		}
		
		"G_PipeIconShadow"
		{
			"ControlName"			"CExLabel"
			"fieldName"				"G_PipeIconShadow"
			"xpos"					"126"
			"ypos"					"1"
			"zpos"					"2"
			"wide"					"16"
			"tall"					"25"
			"textinsetx"			"1"
			"textinsety"			"0"
			"visible"				"0" // 10 CSBC
			"enabled"				"1"
			"labelText"				"G"
			"textAlignment"			"west"
			"font"					"G_HeadsIcon"
			"fgcolor"				"G_Shadow"
		}
		
		"NumPipesLabel"
		{
			"ControlName"	"CExLabel"
			"fieldName"		"NumPipesLabel"
			"xpos"			"20" // 3 CSBC
			"ypos"			"-5" // 4 CSBC
			"zpos"			"2"
			"wide"			"184" // 5 CSBC
			"tall"			"26"
			"textinsetx"	"0" // 6 CSBC
			"textinsety"	"0"
			"autoResize"	"1"
			"pinCorner"		"2"
			"visible"		"1"
			"enabled"		"1"
			"tabPosition"	"0"
			"labelText"		"%activepipes%"
			"textAlignment"	"west" // 7 CSBC
			"dulltext"		"0"
			"brighttext"	"0"
			"font"			"G_Stickys" // 8 CSBC
			"fgcolor"		"G_StickyColor"
		}
		
		"NumPipesLabelDropshadow"
		{
			"ControlName"	"CExLabel"
			"fieldName"		"NumPipesLabelDropshadow"
			"xpos"			"20" // 3 CSBC
			"ypos"			"-5" // 4 CSBC
			"zpos"			"2"
			"wide"			"184" // 5 CSBC
			"tall"			"26"
			"textinsetx"	"0" // 6 CSBC
			"textinsety"	"0"
			"autoResize"	"1"
			"pinCorner"		"2"
			"visible"		"1"
			"enabled"		"1"
			"tabPosition"	"0"
			"labelText"		"8" // 9 CSBC
			"textAlignment"	"west" // 7 CSBC
			"dulltext"		"0"
			"brighttext"	"0"
			"font"			"G_Stickys" // 8 CSBC
			"fgcolor"		"G_Shadow"
		}		
	}
	
	"NoPipesPresentPanel"
	{
		"ControlName"	"EditablePanel"
		"fieldName"		"NoPipesPresentPanel"
		"xpos"			"0"
		"ypos"			"0"
		"zpos"			"1"
		"wide"			"100"
		"tall"			"50"
		"visible"		"0"
		
		"PipeIcon"
		{
			"ControlName"	"CTFImagePanel"
			"fieldName"		"PipeIcon"
			"xpos"			"26"
			"ypos"			"16"
			"ypos_minmode"	"12"
			"wide"			"20"
			"wide_minmode"	"14"
			"tall"			"20"
			"tall_minmode"	"14"
			"visible"		"0"
			"enabled"		"0"
			"image"			"../hud/ico_stickybomb_red_faded"
			"scaleImage"	"1"
			"teambg_2"		"../hud/ico_stickybomb_red_faded"
			"teambg_3"		"../hud/ico_stickybomb_blue_faded"	
		}

		"NumPipesLabel"
		{
			"ControlName"	"CExLabel"
			"fieldName"		"NumPipesLabel"
			"xpos"			"50"
			"xpos_minmode"	"42"
			"ypos"			"18"
			"ypos_minmode"	"12"
			"zpos"			"2"
			"wide"			"35"
			"tall"			"20"
			"tall_lodef"	"28"
			"autoResize"	"1"
			"pinCorner"		"2"
			"visible"		"0"
			"enabled"		"0"
			"tabPosition"	"0"
			"labelText"		"%activepipes%"
			"textAlignment"	"west"
			"dulltext"		"1"
			"brighttext"	"0"
			"font"			"G_FontMedium"
			"font_minmode"	"G_FontMedium"
			"font_lodef"	"G_FontMedium"
		}
		"NumPipesLabelDropshadow"
		{
			"ControlName"	"CExLabel"
			"fieldName"		"NumPipesLabelDropshadow"
			"xpos"			"51"
			"xpos_minmode"	"43"
			"ypos"			"19"
			"ypos_minmode"	"13"
			"zpos"			"2"
			"wide"			"35"
			"tall"			"20"
			"tall_lodef"	"28"
			"autoResize"	"1"
			"pinCorner"		"2"
			"visible"		"0"
			"enabled"		"0"
			"tabPosition"	"0"
			"labelText"		"%activepipes%"
			"textAlignment"	"west"
			"dulltext"		"1"
			"brighttext"	"0"
			"font"			"G_FontMedium"
			"font_minmode"	"G_FontMedium"
			"font_lodef"	"G_FontMedium"
			"fgcolor"		"black"
		}			
	}				
}






















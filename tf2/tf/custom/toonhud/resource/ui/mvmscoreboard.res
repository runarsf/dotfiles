"Resource/UI/MvMScoreboard.res"
{		
	"WaveStatusPanel"
	{
		"ControlName"		"CWaveStatusPanel"
		"fieldName"			"WaveStatusPanel"
		"xpos"				"0"
		"ypos"				"0"
		"zpos"				"0"
		"wide"				"600"
		"tall"				"67"
		"visible"			"1"
		"enabled"			"1"
		"verbose"			"1"
	}
	
	"PopFileLabel"
	{
		"ControlName"	"CExLabel"
		"fieldName"		"PopFileLabel"
		"font"			"G_FontMedium"
		"labelText"		"%popfile%"
		"textAlignment"	"center"
		"xpos"			"3"
		"ypos"			"83"
		"zpos"			"18"
		"wide"			"595"
		"tall"			"20"
		"fgcolor"		"G_White"
		"centerwrap"	"1"
	}

	"DifficultyContainer"
	{
		"ControlName"	"EditablePanel"
		"fieldName"		"DifficultyContainer"
		"xpos"			"3"
		"ypos"			"83"
		"wide"			"f0"
		"tall"			"20"
		"visible"		"1"
		"zpos"			"30"
		
		"DifficultyLabel"
		{
			"ControlName"	"CExLabel"
			"fieldName"		"DifficultyLabel"
			"font"			"G_FontSmall_2"
			"labelText"		"#TF_MvM_Difficulty"
			"textAlignment"	"center"
			"xpos"			"0"
			"ypos"			"0"
			"wide"			"144"
			"tall"			"10"
			"fgcolor"		"G_White"
			"visible"		"0"
		}
		
		"DifficultyValue"
		{
			"ControlName"	"CExLabel"
			"fieldName"		"DifficultyValue"
			"font"			"G_FontSmall_2"
			"labelText"		"%difficultyvalue%"
			"textAlignment"	"center"
			"xpos"			"520"
			"ypos"			"0"
			"wide"			"80"
			"tall"			"20"
			"fgcolor"		"G_White"
		}
	}
	
	"PlayerListBackground"
	{
		"ControlName"	"ScalableImagePanel"
		"fieldName"		"PlayerListBackground"
		"xpos"			"0"
		"ypos"			"75"
		"zpos"			"-1"
		"wide"			"600"
		"tall"			"296"
		"visible"		"0"
		"enabled"		"0"
		"image"			"../hud/tournament_panel_brown"

		"paintborder"		"1"
		"border"			"G_TargetBorder"
		
		"scaleImage"		"1"

		"src_corner_height"	"22"
		"src_corner_width"	"22"
	
		"draw_corner_width"		"0"
		"draw_corner_height" 	"0"	
	}
	
	"PlayersBg"
	{
		"ControlName"	"EditablePanel"
		"fieldName"		"PlayersBg"
		"xpos"			"0"
		"ypos"			"75"
		"zpos"			"-500"
		"wide"			"600"
		"tall"			"370"
		"autoResize"	"0"
		"pinCorner"		"0"
		"visible"		"0"
		"enabled"		"1"
		"paintbackground"		"1"
		"bgcolor_override"		"G_PanelBg"
		"paintborder"			"1"
		"border"				"G_TargetBorder"
	}
	
	"MvMPlayerList"
	{
		"ControlName"	"SectionedListPanel"
		"fieldName"		"MvMPlayerList"
		"xpos"			"10"
		"ypos"			"110"
		"wide"			"582"
		"tall"			"170"
		"zpos"			"2"
		"pinCorner"		"0"
		"visible"		"1"
		"enabled"		"1"
		"tabPosition"	"0"
		"autoresize"	"3"
		"linespacing"	"22"
		"textcolor"		"White"
	}
	
	"WaveStatsBg"
	{
		"ControlName"	"EditablePanel"
		"fieldName"		"WaveStatsBg"
		"xpos"			"10"
		"ypos"			"323"
		"wide"			"284"
		"tall"			"39"
		"visible"		"1"
		"zpos"			"0"
		"paintbackground"	"1"
		"bgcolor_override"	"0 0 0 100"
	}
	
	"TotalStatsBg"
	{
		"ControlName"	"EditablePanel"
		"fieldName"		"TotalStatsBg"
		"xpos"			"306"
		"ypos"			"323"
		"wide"			"284"
		"tall"			"39"
		"visible"		"1"
		"zpos"			"0"
		"paintbackground"	"1"
		"bgcolor_override"	"0 0 0 100"
	}
	
	"PlayerListBg"
	{
		"ControlName"	"EditablePanel"
		"fieldName"		"PlayerListBg"
		"xpos"			"10"
		"ypos"			"110"
		"wide"			"580"
		"tall"			"185"
		"visible"		"1"
		"zpos"			"1"
		"paintbackground"	"1"
		"bgcolor_override"	"0 0 0 100"
	}
	
	"CreditStatsContainer"
	{
		"ControlName"	"EditablePanel"
		"fieldName"		"CreditStatsContainer"
		"xpos"			"10"
		"ypos"			"273"
		"wide"			"582"
		"tall"			"96"
		"zpos"			"1"
		"visible"		"1"
		"paintbackground"	"1"
		"bgcolor_override"	"0 200 0 0"
		
		"CreditStatsBackground"
		{
			"ControlName"	"ScalableImagePanel"
			"fieldName"		"CreditStatsBackground"
			"xpos"			"0"
			"ypos"			"30"
			"zpos"			"-1"
			"wide"			"600"
			"tall"			"66"
			"autoResize"	"0"
			"pinCorner"		"0"
			"visible"		"0"
			"enabled"		"1"
			"image"			"../HUD/tournament_panel_brown"
			
			"paintborder"	"1"
			"border"		"G_TargetBorder"

			"src_corner_height"	"22"
			"src_corner_width"	"22"
		
			"draw_corner_width"		"0"
			"draw_corner_height" 	"0"	
		}
		
		"CreditsLabel"
		{
			"ControlName"	"CExLabel"
			"fieldName"		"CreditsLabel"
			"font"			"G_FontMedium"
			"textinsetx"	"5"
			"labelText"		"#TF_PVE_Currency"
			"textAlignment" "north-west"
			"xpos"			"6"
			"ypos"			"-130"
			"zpos"			"5"
			"wide"			"140"
			"tall"			"40"
			"fgcolor"		"G_White"
			"visible"		"0"
		}
		
		"CreditsShadowLabel"
		{
			"ControlName"	"CExLabel"
			"fieldName"		"CreditsShadowLabel"
			"font"			"G_FontMedium"
			"textinsetx"	"5"
			"labelText"		"#TF_PVE_Currency"
			"textAlignment" "north-west"
			"xpos"			"8"
			"ypos"			"-128"
			"zpos"			"4"
			"wide"			"140"
			"tall"			"40"
			"fgcolor"		"G_Shadow"
			"visible"		"0"
		}
		
		"PreviousWaveCreditInfoPanel"
		{
			"ControlName"	"CCreditDisplayPanel"
			"fieldName"		"PreviousWaveCreditInfoPanel"
			"xpos"			"6"
			"ypos"			"34"
			"wide"			"140"
			"tall"			"54"
			"visible"		"1"
			"paintbackground"	"1"
			"bgcolor_override"	"0 200 0 0"
		}
		
		"PreviousWaveCreditSpendPanel"
		{
			"ControlName"	"CCreditSpendPanel"
			"fieldName"		"PreviousWaveCreditSpendPanel"
			"xpos"			"144"
			"ypos"			"34"
			"wide"			"140"
			"tall"			"54"
			"visible"		"1"
		}
		
		"TotalGameCreditInfoPanel"
		{
			"ControlName"	"CCreditDisplayPanel"
			"fieldName"		"TotalGameCreditInfoPanel"
			"xpos"			"302"
			"ypos"			"34"
			"wide"			"140"
			"tall"			"60"
			"wide"			"200"
			"visible"		"1"
			"paintbackground"	"1"
			"bgcolor_override"	"0 200 0 0"
		}
		
		"TotalGameCreditSpendPanel"
		{
			"ControlName"	"CCreditSpendPanel"
			"fieldName"		"TotalGameCreditSpendPanel"
			"xpos"			"440"
			"ypos"			"34"
			"wide"			"140"
			"tall"			"60"
			"wide"			"200"
			"visible"		"1"
		}
		
		"RespecStatusLabel"
		{
			"ControlName"	"CExLabel"
			"fieldName"		"RespecStatusLabel"
			"font"			"G_FontTiny_2"
			"labelText"		"%respecstatus%"
			"textAlignment" "west"
			"xpos"			"3"
			"ypos"			"5"
			"wide"			"300"
			"tall"			"15"
			"fgcolor"		"G_White"
		}
	}
}

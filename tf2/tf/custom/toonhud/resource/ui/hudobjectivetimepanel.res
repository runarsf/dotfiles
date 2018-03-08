"Resource/UI/HudObjectiveTimePanel.res"
{	
	"TimePanelBG"
	{
		"ControlName"	"ScalableImagePanel"
		"fieldName"		"TimePanelBG"
		"xpos"			"0"
		"ypos"			"0"
		"zpos"			"2"
		"wide"			"60"
		"tall"			"21"
		"visible"		"1"
		"enabled"		"1"
		"image"			"../hud/objectives_timepanel_blue_bg"	
		"scaleImage"	"1"	
		"src_corner_height"		"23"
		"src_corner_width"		"23"
		"draw_corner_width"		"0"
		"draw_corner_height" 	"0"
		
		if_match
		{
			"visible"	"0"
		}
	}
	
	"Gradient"
	{
		"ControlName"		"CTFImagePanel"
		"fieldName"			"Gradient"
		"xpos"				"0"
		"ypos"				"0"
		"zpos"				"3"
		"wide"				"60"
		"tall"				"21"
		"visible"			"1"
		"enabled"			"1"
		"image"				"replay/thumbnails/gradient"
		"scaleImage"		"1"
		
		if_match
		{
			"visible"	"0"
		}
	}
	
	"TimePanelBorder"
	{
		"ControlName"	"EditablePanel"
		"fieldName"		"TimePanelBorder"
		"xpos"			"0"
		"ypos"			"0"
		"zpos"			"4"
		"wide"			"60"
		"tall"			"21"
		"visible"		"1"
		"enabled"		"1"
		"paintborder"	"1"
		"border"		"G_TargetBorder"
		
		if_match
		{
			"visible"	"0"
		}
	}
	"TimePanelProgressBar"
	{
		"ControlName"		"CTFProgressBar"
		"fieldName"			"TimePanelProgressBar"
		"xpos"				"67"
		"ypos"				"16"
		"zpos"				"4"	
		"wide"				"20"
		"tall"				"20"
		"visible"			"0"
		"visible_minmode"	"0"
		"enabled"			"0"
		"scaleImage"		"1"
		"image"				"../hud/objectives_timepanel_progressbar"
		"color_active"		"TimerProgress.Active"
		"color_inactive"	"TimerProgress.InActive"
		"color_warning"		"TimerProgress.Warning"
		"percent_warning"	"0.75"
		
		if_match
		{
			"visible"	"0"
		}
	}
	"WaitingForPlayersLabel"
	{
		"ControlName"	"CExLabel"
		"fieldName"		"WaitingForPlayersLabel"
		"xpos"			"0"
		"ypos"			"21"
		"zpos"			"5"
		"wide"			"60"
		"tall"			"10"
		"visible"		"0"
		"enabled"		"1"
		"labelText"		"#game_WaitingForPlayers"
		"textAlignment"	"center"
		"dulltext"		"0"
		"brighttext"	"0"
		"wrap"			"0"
		"font"			"DefaultVerySmall"
		"font_minmode"	"DefaultVerySmall"
		"textinsety"	"-1"
		"fgcolor_override"	"G_GhostWhite"
		
		if_match
		{
			"proportionaltoparent"	"1"
			"wide"					"76"
		}
	}			
	"WaitingForPlayersBG"
	{
		"ControlName"	"EditablePanel"
		"fieldName"		"WaitingForPlayersBG"
		"xpos"			"0"
		"ypos"			"21"
		"zpos"			"4"
		"wide"			"60"
		"tall"			"10"
		"visible"		"0"
		"enabled"		"1"
		"bgcolor_override"		"G_PanelBg"
		"paintborder"			"1"
		"border"				"G_TimerSetupBorder"
		
		if_match
		{
			"proportionaltoparent"	"1"
			"wide"					"76"
		}
	}
	"OvertimeLabel"
	{
		"ControlName"	"CExLabel"
		"fieldName"		"OvertimeLabel"
		"xpos"			"0"
		"ypos"			"21"
		"zpos"			"5"
		"wide"			"60"
		"tall"			"10"
		"visible"		"0"
		"enabled"		"1"
		"labelText"		"#game_Overtime"
		"textAlignment"	"center"
		"dulltext"		"0"
		"brighttext"	"0"
		"wrap"			"0"
		"font"			"DefaultVerySmall"
		"font_minmode"	"DefaultVerySmall"
		"textinsety"	"-1"
		"fgcolor_override"	"G_GhostWhite"
		
		if_match
		{
			"proportionaltoparent"	"1"
			"wide"					"76"
		}
	}			
	"OvertimeBG"
	{
		"ControlName"	"EditablePanel"
		"fieldName"		"OvertimeBG"
		"xpos"			"0"
		"ypos"			"21"
		"zpos"			"4"
		"wide"			"60"
		"tall"			"10"
		"visible"		"0"
		"enabled"		"1"
		"bgcolor_override"		"G_PanelBg"
		"paintborder"			"1"
		"border"				"G_TimerSetupBorder"
		
		if_match
		{
			"proportionaltoparent"	"1"
			"wide"					"76"
		}
	}
	"SuddenDeathLabel"
	{
		"ControlName"	"CExLabel"
		"fieldName"		"SuddenDeathLabel"
		"xpos"			"0"
		"ypos"			"21"
		"zpos"			"5"
		"wide"			"60"
		"tall"			"10"
		"visible"		"0"
		"enabled"		"1"
		"labelText"		"#game_SuddenDeath"
		"textAlignment"	"center"
		"dulltext"		"0"
		"brighttext"	"0"
		"wrap"			"0"
		"font"			"DefaultVerySmall"
		"font_minmode"	"DefaultVerySmall"
		"textinsety"	"-1"
		"fgcolor_override"	"G_GhostWhite"
		
		if_match
		{
			"proportionaltoparent"	"1"
			"wide"					"76"
		}
	}			
	"SuddenDeathBG"
	{
		"ControlName"	"EditablePanel"
		"fieldName"		"SuddenDeathBG"
		"xpos"			"0"
		"ypos"			"21"
		"zpos"			"4"
		"wide"			"60"
		"tall"			"10"
		"visible"		"0"
		"enabled"		"1"
		"bgcolor_override"		"G_PanelBg"
		"paintborder"			"1"
		"border"				"G_TimerSetupBorder"
		
		if_match
		{
			"proportionaltoparent"	"1"
			"wide"					"76"
		}
	}	
	"SetupLabel"
	{
		"ControlName"	"CExLabel"
		"fieldName"		"SetupLabel"
		"xpos"			"0"
		"ypos"			"21"
		"zpos"			"5"
		"wide"			"60"
		"tall"			"10"
		"visible"		"0"
		"enabled"		"1"
		"labelText"		"#game_Setup"
		"textAlignment"	"center"
		"dulltext"		"0"
		"brighttext"	"0"
		"wrap"			"0"
		"font"			"DefaultVerySmall"
		"font_minmode"	"DefaultVerySmall"
		"textinsety"	"-1"
		"fgcolor_override"	"G_GhostWhite"
		
		if_match
		{
			"proportionaltoparent"	"1"
			"wide"					"76"
		}
	}	
	"SetupBG"
	{
		"ControlName"	"EditablePanel"
		"fieldName"		"SetupBG"
		"xpos"			"0"
		"ypos"			"21"
		"zpos"			"4"
		"wide"			"60"
		"tall"			"10"
		"visible"		"0"
		"enabled"		"1"
		"bgcolor_override"		"G_PanelBg"
		"paintborder"			"1"
		"border"				"G_TimerSetupBorder"

		if_match
		{
			"proportionaltoparent"	"1"
			"wide"					"76"
		}
	}

	"ServerTimeLimitLabel"
	{
		"ControlName"	"CExLabel"
		"fieldName"		"ServerTimeLimitLabel"
		"xpos"			"0"
		"ypos"			"21"
		"zpos"			"1"
		"wide"			"60"
		"tall"			"10"
		"visible"		"0"
		"enabled"		"1"
		"labelText"		"%servertimeleft%"
		"textAlignment"	"center"
		"dulltext"		"0"
		"brighttext"	"0"
		"wrap"			"0"
		"font"			"DefaultVerySmall"
		"font_minmode"	"DefaultVerySmall"
		"textinsety"	"-1"
		"fgcolor_override"	"G_GhostWhite"
		
		if_match
		{
			"proportionaltoparent"	"1"
			"wide"					"76"
		}
	}			
	"ServerTimeLimitLabelBG"
	{
		"ControlName"	"EditablePanel"
		"fieldName"		"ServerTimeLimitLabelBG"
		"xpos"			"0"
		"ypos"			"21"
		"zpos"			"0"
		"wide"			"60"
		"tall"			"10"
		"visible"		"0"
		"enabled"		"1"
		"bgcolor_override"		"G_PanelBg"
		"paintborder"			"1"
		"border"				"G_TimerSetupBorder"
		
		if_match
		{
			"proportionaltoparent"	"1"
			"wide"					"76"
		}
	}
}

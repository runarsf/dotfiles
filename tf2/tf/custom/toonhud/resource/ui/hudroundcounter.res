"Resource/UI/HudRoundCounter.res"
{
	"RoundCounter"
	{
		"fieldName"		"RoundCounter"
		"xpos"			"cs-0.5"
		"ypos"			"-2"
		"zpos"			"2"		
		"wide"			"300"
		"tall"			"25"
		"visible"		"1"
		"enabled"		"1"
		"proportionaltoparent"	"1"

		"starting_width"	"20"
		"width_per_round"	"24"
		"indicator_start_offset"	"5"
		"indicator_max_wide"	"30"

		"RoundIndicatorPanel_kv"
		{
			"ypos"				"3"
			"wide"				"4"
			"tall"				"4"
			"zpos"				"7"
			"image"				"../hud/comp_round_counter_dot_bg"
			"scaleimage"		"1"
			"Alpha"				"150"
		}

		"RoundWinPanelRed_kv"
		{
			"ypos"				"-2"
			"wide"				"13"
			"tall"				"13"
			"zpos"				"8"
			"image"				"../hud/comp_round_counter_light_red"
			"scaleimage"		"1"
		}

		"RoundWinPanelBlue_kv"
		{
			"ypos"				"-2"
			"wide"				"13"
			"tall"				"13"
			"zpos"				"8"
			"image"				"../hud/comp_round_counter_light_blue"
			"scaleimage"		"1"
		}
	}

	"Background"
	{
		"ControlName"	"ImagePanel"
		"fieldName"		"Background"
		"xpos"			"cs-0.5"
		"ypos"			"0"
		"zpos"			"5"
		"wide"			"115"
		"tall"			"30"
		"visible"		"0"

		"backgroundtype"		"2"
		"proportionaltoparent"	"1"
			
		"image"					"../hud/comp_round_timer"
		"scaleimage"			"1"
	}
	
	"BgColor"
	{
		"ControlName"	"EditablePanel"
		"fieldName"		"BgColor"
		"xpos"			"cs-0.5"
		"ypos"			"0"
		"zpos"			"1"
		"wide"			"76"
		"tall"			"23"
		"visible"		"1"
		"enabled"		"1"
		"bgcolor_override"	"G_PanelBg"
		"proportionaltoparent"	"1"
	}
	
	"Borders"
	{
		"ControlName"	"EditablePanel"
		"fieldName"		"Borders"
		"xpos"			"cs-0.5"
		"ypos"			"0"
		"zpos"			"10"
		"wide"			"76"
		"tall"			"23"
		"visible"		"1"
		"enabled"		"1"
		"paintborder"	"1"
		"border"		"G_TargetBorder"
		"proportionaltoparent"	"1"
	}
	
	"Gradient"
	{
		"ControlName"		"CTFImagePanel"
		"fieldName"			"Gradient"
		"xpos"				"cs-0.5"
		"ypos"				"0"
		"zpos"				"9"
		"wide"				"76"
		"tall"				"23"
		"visible"			"1"
		"enabled"			"1"
		"image"				"replay/thumbnails/gradient"
		"scaleImage"		"1"
		"proportionaltoparent"	"1"
	}
	
	"BlueBG"
	{
		"ControlName"	"ScalableImagePanel"
		"fieldName"		"BlueBG"
		"xpos"			"cs-1"
		"ypos"			"0"
		"zpos"			"2"
		"wide"			"38"
		"tall"			"8"
		"visible"		"1"
		"enabled"		"1"
		"image"			"../hud/objectives_timepanel_blue_bg"	
		"scaleImage"	"1"	
		"src_corner_height"		"23"
		"src_corner_width"		"23"
		"draw_corner_width"		"0"
		"draw_corner_height" 	"0"
		"proportionaltoparent"	"1"
	}
	
	"RedBG"
	{
		"ControlName"	"ScalableImagePanel"
		"fieldName"		"RedBG"
		"xpos"			"cs"
		"ypos"			"0"
		"zpos"			"2"
		"wide"			"38"
		"tall"			"8"
		"visible"		"1"
		"enabled"		"1"
		"image"			"../hud/objectives_timepanel_red_bg"
		"scaleImage"	"1"	
		"src_corner_height"		"23"
		"src_corner_width"		"23"
		"draw_corner_width"		"0"
		"draw_corner_height" 	"0"
		"proportionaltoparent"	"1"
	}
	
	"TopBG"
	{
		"ControlName"		"CTFImagePanel"
		"fieldName"			"TopBG"
		"xpos"				"cs-0.5"
		"ypos"				"0"
		"zpos"				"9"
		"wide"				"76"
		"tall"				"8"
		"visible"			"1"
		"enabled"			"1"
		"image"				"replay/thumbnails/gradient"
		"scaleImage"		"1"
		"proportionaltoparent"	"1"
	}
}

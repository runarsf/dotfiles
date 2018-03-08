"Resource/UI/WaveStatusPanel.res"
{
	"Background"
	{
		"ControlName"	"ScalableImagePanel"
		"fieldName"		"Background"
		"xpos"			"150"
		"ypos"			"2"
		"zpos"			"-1"
		"wide"			"200"
		"tall"			"0"
		"autoResize"	"1"
		"pinCorner"		"0"
		"visible"		"0"
		"enabled"		"1"
		"image"			"../HUD/tournament_panel_brown"
		"paintborder"			"1"
		"border"				"G_TargetBorder"
		"src_corner_height"		"23"
		"src_corner_width"		"23"
		"draw_corner_width"		"0"
		"draw_corner_height" 	"0"	
	}
	
	"WaveCountLabel"
	{
		"ControlName"			"CExLabel"
		"fieldName"				"WaveCountLabel"
		"font"					"DefaultVerySmall"
		"fgcolor_override"		"G_White"
		"xpos"					"200"
		"ypos"					"12"
		"ypos_minmode"			"3"
		"zpos"					"12"
		"wide"					"200"
		"tall"					"12"
		"visible"				"1"
		"enabled"				"1"
		"textAlignment"			"center"
		"labelText"				"%wave_count%"
	}
	
	"SeparatorBar"
	{
		"ControlName"	"Panel"
		"fieldName"		"SeparatorBar"
		"xpos"			"0"
		"ypos"			"0"
		"zpos"			"3"
		"wide"			"1"
		"tall"			"30"
		"visible"		"0"
		"enabled"		"1"
		"scaleImage"	"1"
		"PaintBackgroundType" "2"
		"bgcolor_override"	"TanLight"
		
		if_verbose
		{
			"visible"		"1"
		}
	}
	
	"SupportLabel"
	{
		"ControlName"			"CExLabel"
		"fieldName"				"SupportLabel"
		"font"					"G_FontSmall_2"
		"fgcolor"				"G_White"
		"xpos"					"55"
		"ypos"					"6"
		"zpos"					"3"
		"wide"					"60"
		"tall"					"15"
		"visible"				"0"
		"enabled"				"1"
		"textAlignment"			"west"
		"labelText"				"#TF_MVM_Support"
		
		if_verbose
		{
			"visible"		"1"
		}		
	}
	
	"Gradient"
	{
		"ControlName"		"CTFImagePanel"
		"fieldName"			"Gradient"
		"xpos"				"205"
		"ypos"				"12"
		"ypos_minmode"		"3"
		"zpos"				"10"
		"wide"				"190"
		"tall"				"12"
		"visible"			"1"
		"enabled"			"1"
		"image"				"replay/thumbnails/gradient"
		"scaleImage"		"1"
	}
	
	"ProgressBar"
	{
		"ControlName"	"ScalableImagePanel"
		"fieldName"		"ProgressBar"
		"xpos"			"205"
		"ypos"			"12"
		"ypos_minmode"	"3"
		"zpos"			"4"
		"wide"			"188"
		"tall"			"12"
		"visible"		"1"
		"enabled"		"1"
		"image"			"../hud/color_panel_blu"
		"src_corner_height"		"23"
		"src_corner_width"		"23"
		"draw_corner_width"		"0"
		"draw_corner_height" 	"0"		
	}
	
	"ProgressBarBG"
	{
		"ControlName"	"ScalableImagePanel"
		"fieldName"		"ProgressBarBG"
		"xpos"			"205"
		"ypos"			"12"
		"ypos_minmode"	"3"
		"zpos"			"3"
		"wide"			"188"
		"tall"			"12"
		"visible"		"1"
		"enabled"		"1"
		"image"			"../HUD/tournament_panel_brown"
		"paintborder"			"0"
		"border"				"G_MeterBorder"
		"src_corner_height"		"23"
		"src_corner_width"		"23"
		"draw_corner_width"		"0"
		"draw_corner_height" 	"0"	
	}
	
	"ProgressBarBorder"
	{
		"ControlName"	"EditablePanel"
		"fieldName"		"ProgressBarBorder"
		"xpos"			"205"
		"ypos"			"12"
		"ypos_minmode"	"3"
		"zpos"			"20"
		"wide"			"190"
		"tall"			"12"
		"visible"		"1"
		"enabled"		"1"
		"paintborder"	"1"
		"border"		"G_TargetBorder"
	}
}

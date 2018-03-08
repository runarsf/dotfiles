"Resource/UI/HudStopWatch.res"
{
	"HudStopWatchBG"
	{
		"ControlName"	"EditablePanel"
		"fieldName"		"HudStopWatchBG"
		"xpos"			"0"
		"ypos"			"0"
		"zpos"			"-1"
		"wide"			"130"
		"tall"			"21"
		"tall_minmode"	"15"
		"autoResize"	"0"
		"pinCorner"		"0"
		"visible"		"1"
		"enabled"		"1"
		"paintbackground"	"1"
		"bgcolor_override"	"G_PanelBg"
		"paintborder"		"1"
		"border"			"G_TargetBorderNoTop"
		
		if_comp
		{
			"xpos"			"132"
			"ypos"			"32"
			"wide"			"76"
			"tall"			"12"
			"tall_minmode"	"12"
			"paintborder"	"0"
		}
	}

	"StopWatchImageCaptureTime"
	{
		"ControlName"	"ImagePanel"
		"fieldName"		"StopWatchImageCaptureTime"
		"xpos"			"5"
		"ypos"			"3"
		"ypos_minmode"	"1"
		"zpos"			"10"
		"wide"			"15"
		"tall"			"15"
		"wide_minmode"	"12"
		"tall_minmode"	"12"
		"visible"		"1"
		"enabled"		"1"
		"image"			"../hud/ico_time_10"
		"scaleImage"	"1"
		
		if_comp
		{
			"visible"		"0"
		}
	}

	"ObjectiveStatusTimePanel"
	{
		"ControlName"		"EditablePanel"
		"fieldName"			"ObjectiveStatusTimePanel"
		"xpos"				"0"
		"ypos"				"0"
		"zpos"				"1"
		"wide"				"60"
		"tall"				"21"
		"tall_minmode"		"15"
		"visible"			"0"
		"enabled"			"1"
		"paintbackground"	"1"
		"bgcolor_override"	"G_Shadow"
		
		if_comp
		{
			"xpos"				"132"
			"ypos"				"32"
			"wide"				"26"
			"tall"				"12"
			"tall_minmode"		"12"
			"bgcolor_override"	"0 0 0 0"
		}

		"TimePanelValue"
		{
			"ControlName"	"CExLabel"
			"fieldName"		"TimePanelValue"
			"font"			"G_FontSmall"
			"font_minmode"	"G_FontSmall"
			"fgcolor"		"G_White"
			"xpos"			"25"
			"xpos_minmode"	"22"
			"ypos"			"0"
			"ypos_minmode"	"0"
			"zpos"			"3"
			"wide"			"45"
			"wide_minmode"	"45"
			"tall"			"21"
			"tall_minmode"	"15"
			"visible"		"1"
			"enabled"		"1"
			"textAlignment"	"west"
			"textinsetx"	"5"
			
			if_comp
			{
				"font"			"DefaultVerySmall"
				"font_minmode"	"DefaultVerySmall"
				"textAlignment"	"center"
				"xpos"			"0"
				"xpos_minmode"	"0"
				"wide"			"26"
				"tall"			"12"
				"tall_minmode"	"12"
			}
		}	
	}

	"StopWatchScoreToBeat"
	{
		"ControlName"	"CExLabel"
		"fieldName"		"StopWatchScoreToBeat"
		"font"			"G_FontSmall"
		"labelText"		"%scoretobeat%"
		"textAlignment"	"east"
		"xpos"			"0"
		"ypos"			"0"
		"zpos"			"4"
		"wide"			"77"
		"tall"			"21"
		"tall_minmode"	"15"
		"autoResize"	"0"
		"pinCorner"		"0"
		"visible"		"0"
		"enabled"		"1"
		"fgcolor"		"G_White"
		"textinsetx"	"5"
		
		if_comp
		{
			"font"			"DefaultVerySmall"
			"xpos"			"159"
			"ypos"			"32"
			"wide"			"10"
			"tall"			"12"
			"tall_minmode"	"12"
		}
	}
	"StopWatchPointsLabel"
	{
		"ControlName"	"CExLabel"
		"fieldName"		"StopWatchPointsLabel"
		"font"			"G_FontTiny_2"
		"labelText"		"%pointslabel%"
		"textAlignment"	"west"
		"xpos"			"77"
		"ypos"			"1"
		"zpos"			"4"
		"wide"			"95"
		"tall"			"21"
		"tall_minmode"	"15"
		"autoResize"	"0"
		"pinCorner"		"0"
		"visible"		"1"
		"enabled"		"1"
		"wrap"			"0"
		"fgcolor"		"G_White"
		
		if_comp
		{
			"font"			"DefaultVerySmall"
			"xpos"			"169"
			"ypos"			"32"
			"wide"			"25"
			"tall"			"12"
			"tall_minmode"	"12"
		}
	}

	"StopWatchLabel"
	{
		"ControlName"	"CExLabel"
		"fieldName"		"StopWatchLabel"
		"font"			"G_FontTiny_2"
		"labelText"		"%stopwatchlabel%"
		"textAlignment"	"west"
		"xpos"			"21"
		"ypos"			"0"
		"zpos"			"4"
		"wide"			"130"
		"tall"			"21"
		"tall_minmode"	"15"
		"autoResize"	"0"
		"pinCorner"		"0"
		"visible"		"0"
		"enabled"		"1"
		"wrap"			"1"
		"fgcolor"		"G_White"
		
		if_comp
		{
			"font"			"DefaultVerySmall"
			"xpos"			"132"
			"ypos"			"32"
			"wide"			"76"
			"tall"			"12"
			"tall_minmode"	"12"
			"textAlignment"	"center"
			"wrap"			"0"
			"bgcolor_override"	"200 0 0 0"
		}
	}

	"HudStopWatchDescriptionBG"
	{
		"ControlName"	"ScalableImagePanel"
		"fieldName"		"HudStopWatchDescriptionBG"
		"xpos"			"0"
		"ypos"			"21"
		"ypos_minmode"	"15"
		"zpos"			"-1"
		"wide"			"130"
		"tall"			"10"
		"autoResize"	"0"
		"pinCorner"		"0"
		"visible"		"0"
		"enabled"		"1"
		"image"			"../hud/objectives_timepanel_suddendeath"
		"paintborder"	"1"
		"border"		"G_TargetBorder"
		"src_corner_height"		"23"
		"src_corner_width"		"23"
		"draw_corner_width"		"0"
		"draw_corner_height"	"0"
		
		if_comp
		{
			"ypos"	"-9999"
		}
	}

	"StopWatchDescriptionLabel"
	{
		"ControlName"	"CExLabel"
		"fieldName"		"StopWatchDescriptionLabel"
		"font"			"DefaultVerySmall"
		"labelText"		"%descriptionlabel%"
		"textAlignment"	"center"
		"xpos"			"0"
		"ypos"			"21"
		"ypos_minmode"	"15"
		"zpos"			"4"
		"wide"			"130"
		"tall"			"10"
		"autoResize"	"0"
		"pinCorner"		"0"
		"visible"		"0"
		"enabled"		"1"
		"wrap"			"0"
		
		if_comp
		{
			"ypos"	"-9999"
		}
	}
}
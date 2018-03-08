"Resource/UI/HudArenaPlayerCount.res"
{
	"Borders"
	{
		"ControlName"	"EditablePanel"
		"fieldName"		"Borders"
		"xpos"			"c-47"
		"ypos"			"0"
		"zpos"			"100"
		"wide"			"94"
		"tall"			"21"
		"tall_minmode"	"15"
		"autoResize"	"0"
		"pinCorner"		"0"
		"visible"		"1"
		"enabled"		"1"
		"paintborder"	"1"
		"border"		"G_TargetBorder"
	}
	
	"Gradient"
	{
		"ControlName"		"CTFImagePanel"
		"fieldName"			"Gradient"
		"xpos"				"c-47"
		"ypos"				"0"
		"zpos"				"99"
		"wide"				"94"
		"tall"				"21"
		"tall_minmode"		"15"
		"visible"			"1"
		"enabled"			"1"
		"image"				"replay/thumbnails/gradient"
		"scaleImage"		"1"
	}
		
	"blueteam"
	{
		"ControlName"	"EditablePanel"
		"fieldName"		"blueteam"
		"xpos"			"c-47"
		"ypos"			"0"
		"zpos"			"0"
		"wide"			"38"
		"tall"			"30"
		"visible"		"1"
	
		"background"
		{
			"ControlName"	"CTFImagePanel"
			"fieldName"		"background"
			"xpos"			"0"
			"ypos"			"0"
			"zpos"			"0"
			"wide"			"38"
			"tall"			"21"
			"tall_minmode"	"15"
			"autoResize"	"0"
			"pinCorner"		"0"
			"visible"		"1"
			"enabled"		"1"
			"image"			"../hud/color_panel_blu"
			"paintborder"	"0"
			"border"		"G_TargetBorder"
				
			"src_corner_height"		"20"			// pixels inside the image
			"src_corner_width"		"20"
					
			"draw_corner_width"		"0"				// screen size of the corners ( and sides ), proportional
			"draw_corner_height" 	"0"	
		}
		
		"count"
		{
			"ControlName"	"CExLabel"
			"fieldName"		"count"
			"xpos"			"0"
			"ypos"			"0"
			"zpos"			"2"
			"wide"			"38"
			"tall"			"21"
			"tall_minmode"	"15"
			"autoResize"	"0"
			"pinCorner"		"0"
			"visible"		"1"
			"enabled"		"1"
			"font"			"G_FontMedium"
			"font_minmode"	"G_FontSmall"
			"labelText"		"%blue_alive%"
			"textAlignment"	"center"
			"fgcolor"		"G_White"
			"textinsety"	"-1"
		}	
		"countshadow"
		{
			"ControlName"	"CExLabel"
			"fieldName"		"countshadow"
			"xpos"			"2"
			"ypos"			"2"
			"zpos"			"2"
			"wide"			"38"
			"tall"			"20"
			"autoResize"	"0"
			"pinCorner"		"0"
			"visible"		"0"
			"enabled"		"0"
			"font"			"G_FontMedium"
			"labelText"		"%blue_alive%"
			"textAlignment"	"center"
			"fgcolor"		"G_Shadow"
		}
		
		"playerimage"
		{
			"ControlName"	"ImagePanel"		
			"fieldName"		"playerimage"
			"xpos"			"12"
			"ypos"			"10"
			"zpos"			"3"
			"wide"			"8"
			"tall"			"16"
			"tall_minmode"	"12"
			"visible"		"0"
			"enabled"		"0"
			"image"			"capture_icon_white"
			"scaleImage"	"1"
		}
	}
	
	"PlayerIconBg"
	{
		"ControlName"	"CTFImagePanel"
		"fieldName"		"PlayerIconBg"
		"xpos"			"c-10"
		"ypos"			"0"
		"zpos"			"2"
		"wide"			"20"
		"tall"			"21"
		"tall_minmode"	"15"
		"autoResize"	"0"
		"pinCorner"		"0"
		"visible"		"1"
		"enabled"		"1"
		"image"			"../hud/color_panel_brown_opaque"
		"paintborder"	"0"
		"border"		"G_TargetBorder"
			
		"src_corner_height"		"20"			// pixels inside the image
		"src_corner_width"		"20"
				
		"draw_corner_width"		"0"				// screen size of the corners ( and sides ), proportional
		"draw_corner_height" 	"0"	
	}
	
	"PlayerIcon"
	{
		"ControlName"	"ImagePanel"		
		"fieldName"		"PlayerIcon"
		"xpos"			"c-4"
		"xpos_minmode"	"c-3"
		"ypos"			"2"
		"ypos_minmode"	"1"
		"zpos"			"3"
		"wide"			"8"
		"wide_minmode"	"6"
		"tall"			"16"
		"tall_minmode"	"12"
		"visible"		"1"
		"enabled"		"1"
		"image"			"capture_icon_white"
		"scaleImage"	"1"
	}	
	
	

	"redteam"
	{
		"ControlName"	"EditablePanel"
		"fieldName"		"redteam"
		"xpos"			"c9"
		"ypos"			"0"
		"zpos"			"0"
		"wide"			"38"
		"tall"			"30"
		"visible"		"1"
	
		"background"
		{
			"ControlName"	"CTFImagePanel"
			"fieldName"		"background"
			"xpos"			"0"
			"ypos"			"0"
			"zpos"			"0"
			"wide"			"38"
			"tall"			"21"
			"tall_minmode"	"15"
			"autoResize"	"0"
			"pinCorner"		"0"
			"visible"		"1"
			"enabled"		"1"
			"image"			"../hud/color_panel_red"
			"paintborder"	"0"
			"border"		"G_TargetBorder"
				
			"src_corner_height"		"20"			// pixels inside the image
			"src_corner_width"		"20"
					
			"draw_corner_width"		"0"				// screen size of the corners ( and sides ), proportional
			"draw_corner_height" 	"0"	
		}
		
		"count"
		{
			"ControlName"	"CExLabel"
			"fieldName"		"count"
			"xpos"			"0"
			"ypos"			"0"
			"zpos"			"2"
			"wide"			"38"
			"tall"			"21"
			"tall_minmode"	"15"
			"autoResize"	"0"
			"pinCorner"		"0"
			"visible"		"1"
			"enabled"		"1"
			"font"			"G_FontMedium"
			"font_minmode"	"G_FontSmall"
			"labelText"		"%red_alive%"
			"textAlignment"	"center"
			"fgcolor"		"G_White"
			"textinsety"	"-1"
		}	
		"countshadow"
		{
			"ControlName"	"CExLabel"
			"fieldName"		"countshadow"
			"xpos"			"2"
			"ypos"			"2"
			"zpos"			"2"
			"wide"			"38"
			"tall"			"20"
			"autoResize"	"0"
			"pinCorner"		"0"
			"visible"		"0"
			"enabled"		"0"
			"font"			"G_FontMedium"
			"labelText"		"%red_alive%"
			"textAlignment"	"center"
			"fgcolor"		"G_Shadow"
		}
	
		"playerimage"
		{
			"ControlName"	"ImagePanel"		
			"fieldName"		"playerimage"
			"xpos"			"12"
			"ypos"			"10"
			"zpos"			"3"
			"wide"			"8"
			"tall"			"16"
			"visible"		"0"
			"enabled"		"0"
			"image"			"capture_icon_white"
			"scaleImage"	"1"
		}			
	}
}

"Resource/UI/HudMiniGame_Base.res"
{
	"G_Borders"
	{
		"ControlName"		"EditablePanel"
		"fieldName"			"G_Borders"
		"xpos"				"c-60"
		"ypos"				"r30"
		"zpos"				"50"
		"wide"				"120"
		"tall"				"20"
		"paintborder"		"1"
		"border"			"G_TargetBorder"
		"visible"			"1"
		"enabled"			"1"
	}
	
	"Gradient"
	{
		"ControlName"		"CTFImagePanel"
		"fieldName"			"Gradient"
		"xpos"				"c-60"
		"ypos"				"r30"
		"zpos"				"5"
		"wide"				"120"
		"tall"				"20"
		"visible"			"1"
		"enabled"			"1"
		"image"				"replay/thumbnails/gradient"
		"scaleImage"		"1"
	}

	"G_RedPanel"
	{
		"ControlName"	"EditablePanel"
		"fieldName"		"G_RedPanel"
		"xpos"			"c-10"
		"ypos"			"r30"
		"wide"			"70"
		"tall"			"20"
		"zpos"			"3"
		"autoResize"	"0"
		"pinCorner"		"0"
		"visible"		"1"
		"enabled"		"1"
		"paintbackground"	"1"
		"bgcolor_override"	"G_TeamRed"
	}
	
	"G_Slash"
	{
		"ControlName"	"CExLabel"
		"fieldName"		"G_Slash"
		"font"			"G_Slash"
		"labelText"		"/"
		"textAlignment"	"west"
		"xpos"			"c-10" [$WINDOWS]
		"xpos"			"c-11" [!$WINDOWS]
		"ypos"			"r30"
		"wide"			"20"
		"tall"			"20" [$WINDOWS]
		"tall"			"21" [!$WINDOWS]
		"zpos"			"4"
		"autoResize"	"0"
		"pinCorner"		"0"
		"visible"		"1"
		"enabled"		"1"
		"fgcolor"		"G_TeamBlue"
	}
	
	"G_BluePanel"
	{
		"ControlName"	"EditablePanel"
		"fieldName"		"G_BluePanel"
		"xpos"			"c-60"
		"ypos"			"r30"
		"wide"			"51"
		"tall"			"20"
		"zpos"			"2"
		"autoResize"	"0"
		"pinCorner"		"0"
		"visible"		"1"
		"enabled"		"1"
		"paintbackground"	"1"
		"bgcolor_override"	"G_TeamBlue"
	}
	
	"LeftSideBG"
	{
		"ControlName"	"ImagePanel"
		"fieldName"		"LeftSideBG"
		"xpos"			"cs-0.5"
		"ypos"			"r77"
		"zpos"			"1"
		"wide"			"280"
		"tall"			"80"
		"visible"		"0"
		"enabled"		"0"
		"image"			"../hud/objectives_flagpanel_bg_left"
		"scaleImage"	"1"	
	}

	"RightSideBG"
	{
		"ControlName"	"ImagePanel"
		"fieldName"		"RightSideBG"
		"xpos"			"cs-0.5"
		"ypos"			"r77"
		"zpos"			"1"
		"wide"			"280"
		"tall"			"80"
		"visible"		"0"
		"enabled"		"0"
		"image"			"../hud/objectives_flagpanel_bg_right"
		"scaleImage"	"1"	
	}
		
	"OutlineBG"
	{
		"ControlName"	"ImagePanel"
		"fieldName"		"OutlineBG"
		"xpos"			"cs-0.5"
		"ypos"			"r77"
		"zpos"			"2"
		"wide"			"280"
		"tall"			"80"
		"visible"		"0"
		"enabled"		"0"
		"image"			"../hud/objectives_flagpanel_bg_outline"
		"scaleImage"	"1"	
	}
	
	"BlueScore"
	{
		"ControlName"	"CExLabel"
		"fieldName"		"BlueScore"
		"xpos"			"c-60"
		"ypos"			"r30"
		"zpos"			"9"
		"wide"			"50"
		"tall"			"20"
		"visible"		"1"
		"enabled"		"1"
		"textAlignment"	"center"
		"textinsetx"	"20"
		"labelText"		"%bluescore%"
		"font"			"G_FontMedium"
		"fgcolor"		"G_White"
		
		"if_hybrid"
		{
			"visible"	"0"
		}
		
		"if_mvm"
		{
			"visible"	"0"
		}
		
		"if_specialdelivery"
		{
			"visible"	"0"
		}
	}	
		
	"BlueScoreShadow"
	{
		"ControlName"	"CExLabel"
		"fieldName"		"BlueScoreShadow"
		"xpos"			"c-58"
		"ypos"			"r23"
		"zpos"			"8"
		"wide"			"46"
		"tall"			"20"
		"visible"		"0"
		"enabled"		"0"
		"textAlignment"	"center"
		"textinsetx"	"20"		
		"labelText"		"%bluescore%"
		"font"			"G_FontMedium"
		"fgcolor"		"G_Shadow"
	}	
							
	"RedScore"
	{
		"ControlName"	"CExLabel"
		"fieldName"		"RedScore"
		"xpos"			"c+10"
		"ypos"			"r30"
		"zpos"			"9"
		"wide"			"50"
		"tall"			"20"
		"visible"		"1"
		"enabled"		"1"
		"textAlignment"	"center"
		"textinsetx"	"20"
		"labelText"		"%redscore%"
		"font"			"G_FontMedium"
		"fgcolor"		"G_White"		
		
		"if_hybrid"
		{
			"visible"	"0"
		}
		
		"if_mvm"
		{
			"visible"	"0"
		}
		
		"if_specialdelivery"
		{
			"visible"	"0"
		}
	}	
		
	"RedScoreShadow"
	{
		"ControlName"	"CExLabel"
		"fieldName"		"RedScoreShadow"
		"xpos"			"c16"
		"ypos"			"r23"
		"zpos"			"9"
		"wide"			"46"
		"tall"			"20"
		"visible"		"0"
		"enabled"		"0"
		"textAlignment"	"center"	
		"textinsetx"	"20"
		"labelText"		"%redscore%"
		"font"			"G_FontMedium"
		"fgcolor"		"G_Shadow"		
	}
															
	"PlayingTo"
	{
		"ControlName"	"CExLabel"
		"fieldName"		"PlayingTo"
		"xpos"			"c-60"
		"ypos"			"r10"
		"zpos"			"12"
		"wide"			"120"
		"tall"			"10"
		"visible"		"1"
		"enabled"		"1"
		"labelText"		"#TF_PlayingTo"
		"textAlignment"	"center"
		"dulltext"		"0"
		"brighttext"	"0"
		"font"			"DefaultVerySmall"
		"fgcolor"		"G_White"
		"centerwrap"	"1"
	}	
			
	"PlayingToBG"
	{
		"ControlName"	"CTFImagePanel"
		"fieldName"		"PlayingToBG"
		"xpos"			"c-15"
		"ypos"			"r10"
		"zpos"			"10"
		"wide"			"29"
		"tall"			"10"
		"visible"		"0"
		"enabled"		"0"
		"image"			"../hud/objectives_flagpanel_bg_playingto"
		"scaleImage"	"1"
		"paintborder"	"1"
		"border"		"G_TargetBorder"
		"src_corner_height"		"23"
		"src_corner_width"		"23"
		"draw_corner_width"		"0"
		"draw_corner_height" 	"0"	
		"Alpha"	"0"
	}	

	"GameImage"
	{
		"ControlName"	"ImagePanel"
		"fieldName"		"GameImage"
		"xpos"			"cs-0.5"
		"ypos"			"r70"
		"zpos"			"5"
		"wide"			"40"
		"tall"			"40"
		"visible"		"1"
		"enabled"		"1"
		"image"			""
		"scaleImage"	"1"	
	}
}

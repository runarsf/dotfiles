"Resource/UI/TargetID.res"
{
	"TargetIDBG"
	{
		"ControlName"			"CTFImagePanel"
		"fieldName"				"TargetIDBG"
		"xpos"					"0"
		"ypos"					"27" // 1 CTID
		"zpos"					"0" // 2 CTID
		"wide"					"252"
		"tall"	 				"50"
		"autoResize"			"0"
		"pinCorner"				"0"
		"visible"				"1"
		"enabled"				"1"
		"image"					"../hud/color_panel_brown"
		"scaleImage"			"1"
		"teambg_1"				"../hud/color_panel_brown"
		"teambg_2"				"../hud/color_panel_red"
		"teambg_3"				"../hud/color_panel_blu"
		"paintborder"			"0" // 2 CTID
		"border"				"G_TargetBorder"
		"src_corner_height"		"23"
		"src_corner_width"		"23"
		"draw_corner_width"		"0"
		"draw_corner_height" 	"0"	
	}
	
	"TargetIDShader"
	{
		"ControlName"			"EditablePanel"
		"fieldName"				"TargetIDShader"
		"xpos"					"0"
		"ypos"					"0"
		"zpos"					"-1"
		"wide"					"f0"
		"tall"	 				"40"
		"autoResize"			"0"
		"pinCorner"				"0"
		"visible"				"1" // 3 CTID
		"enabled"				"1"
		"paintbackground"		"1"
		"bgcolor_override"		"G_CustomTargetID"
	}
	
	"TargetIDBG_Spec_Blue"
	{
		"ControlName"	"ScalableImagePanel"
		"fieldName"		"TargetIDBG_Spec_Blue"
		"xpos"			"0"
		"ypos"			"27" // 1 CTID
		"zpos"			"-1"
		"wide"			"252"
		"tall"	 		"30"
		"autoResize"	"0"
		"pinCorner"		"0"
		"visible"		"0"
		"enabled"		"1"
		"image"			"../hud/color_panel_blu"
		"paintborder"	"0" // 2 CTID
		"border"		"G_TargetBorder"
		"src_corner_height"		"23"
		"src_corner_width"		"23"
		"draw_corner_width"		"0"
		"draw_corner_height" 	"0"
	}
	"TargetIDBG_Spec_Red"
	{
		"ControlName"	"ScalableImagePanel"
		"fieldName"		"TargetIDBG_Spec_Red"
		"xpos"			"0"
		"ypos"			"27" // 1 CTID
		"zpos"			"-1"
		"wide"			"252"
		"tall"	 		"30"
		"autoResize"	"0"
		"pinCorner"		"0"
		"visible"		"0"
		"enabled"		"1"
		"image"			"../hud/color_panel_red"
		"paintborder"	"0" // 2 CTID
		"border"		"G_TargetBorder"
		"src_corner_height"		"23"
		"src_corner_width"		"23"
		"draw_corner_width"		"0"
		"draw_corner_height" 	"0"
	}
	
	"TargetNameLabel"
	{	
		"ControlName"		"Label"
		"fieldName"			"TargetNameLabel"
		"font"				"G_FontSmall_2"
		"xpos"				"30"
		"ypos"				"6"
		"zpos"				"1"
		"wide"				"640"
		"tall"				"12"
		"autoResize"		"0"
		"pinCorner"			"0"
		"visible"			"1"
		"enabled"			"1"
		"labelText"			"%targetname%"
		"textAlignment"		"North-West"
		"dulltext"			"0"
		"brighttext"		"0"
		"fgcolor_override"	"G_White"
		"textinsetx"		"1"
	}
	
	"TargetNameLabelShadow"
	{	
		"ControlName"		"Label"
		"fieldName"			"TargetNameLabelShadow"
		"font"				"G_FontSmall_2"
		"xpos"				"31"
		"ypos"				"7"
		"zpos"				"1"
		"wide"				"640"
		"tall"				"12"
		"autoResize"		"0"
		"pinCorner"			"0"
		"visible"			"0"
		"enabled"			"0"
		"labelText"			"%targetname%"
		"textAlignment"		"North-West"
		"dulltext"			"0"
		"brighttext"		"0"
		"fgcolor_override"	"G_Shadow"
		"textinsetx"		"1"
	}
	
	"TargetDataLabel"
	{	
		"ControlName"	"Label"
		"fieldName"		"TargetDataLabel"
		"font"			"DefaultVerySmall"
		"xpos"			"28"
		"ypos"			"18"
		"zpos"			"1"
		"wide"			"280"
		"tall"			"8"
		"autoResize"	"0"
		"pinCorner"		"0"
		"visible"		"1"
		"enabled"		"1"
		"labelText"		"%targetdata%"
		"textAlignment"	"North-West"
		"dulltext"		"0"
		"brighttext"	"0"
		"textinsetx"		"6"
		"fgcolor_override"	"G_White"
	}
	

	"SpectatorGUIHealth"
	{
		"ControlName"	"EditablePanel"
		"fieldName"		"SpectatorGUIHealth"
		"xpos"			"3" // 1 CTHB
		"ypos"			"3" // 2 CTHB
		"wide"			"23" // 3 CTHB
		"tall"			"23" // 3 CTHB
		"visible"		"1"
		"enabled"		"1"	
		"HealthBonusPosAdj"			"0" // 4 CTHB
		"HealthDeathWarning"		"0.49"
		"TFFont"					"G_FontTiny"
		"HealthDeathWarningColor"	"HUDDeathWarning"
		"TextColor"					"G_White"
		"paintborder"				"1" // 5 CTHB
		"paintbackground"			"1" // 5 CTHB
		"border"					"G_HealthBorder"
		"bgcolor_override"			"G_HealthBarBg"
	}	
	
	"AmmoIcon"
	{
		"ControlName"	"ImagePanel"
		"fieldName"		"AmmoIcon"
		"xpos"			"35" // 6 CTHB
		"ypos"			"19"
		"zpos"			"12"
		"wide"			"6"
		"tall"			"6"
		"visible"		"0"
		"enabled"		"1"
		"scaleImage"	"1"
		"image"			"../hud/hud_obj_status_ammo_64"
		"drawcolor"		"G_White"
	}
	
	"KillStreakIcon"
	{
		"ControlName"	"ImagePanel"
		"fieldName"		"KillStreakIcon"
		"xpos"			"35" // 6 CTHB
		"ypos"			"19"
		"zpos"			"12"
		"wide"			"6"
		"tall"			"6"
		"visible"		"0"
		"enabled"		"1"
		"scaleImage"	"1"
		"image"			"../hud/leaderboard_streak"
		"scaleImage"	"1"
		"drawcolor"		"G_White"
	}
	
	"MoveableSubPanel"
	{
		"ControlName"	"EditablePanel"
		"fieldName"		"MoveableSubPanel"
		"xpos"					"-999"
		"ypos"					"0"
		"zpos"					"5"
		"wide"					"32"
		"tall"					"36"
		"visible"				"1"
		"enabled"				"1"	
		"paintbackground"		"0" // 2 CTID
		"paintborder"			"0" // 2 CTID
		"border"				"G_TargetBorder"
		"bgcolor_override"		"G_Grey"
		
		"MoveableIconBG"
		{
			"ControlName"	"CIconPanel"
			"fieldName"		"MoveableIconBG"
			"xpos"			"0"
			"ypos"			"0"
			"zpos"			"0"
			"wide"			"10"
			"tall"			"36"
			"visible"		"0"
			"enabled"		"0"
			"icon"			"obj_status_alert_background_tall_nocolor"
			"iconColor"		"HudBlack"
			"scaleImage"	"1"
		}
		
		"MoveableIcon"
		{
			"ControlName"	"CIconPanel"
			"fieldName"		"MoveableIcon"
			"xpos"			"5"
			"ypos"			"-3"
			"zpos"			"11"
			"wide"			"14"
			"tall"			"14"
			"visible"		"0"
			"enabled"		"0"
			"icon"			"obj_status_sentrygun_1"
			"drawcolor"		"ProgressOffWhite"
			"scaleImage"	"1"
		}
		"MoveableSymbolIcon"
		{
			"ControlName"	"ImagePanel"
			"fieldName"		"MoveableSymbolIcon"
			"xpos"			"16"
			"ypos"			"-3"
			"zpos"			"12"
			"wide"			"16"
			"tall"			"8"
			"visible"		"1"
			"enabled"		"1"
			"image"			"../hud/eng_sel_item_movable"
			"drawcolor"		"ProgressOffWhite"
			"scaleImage"	"1"
		}

		"MoveableKeyLabel"
		{	
			"ControlName"	"Label"
			"fieldName"		"MoveableKeyLabel"
			"font"			"DefaultVerySmall"
			"xpos"			"0"
			"ypos"			"20"
			"zpos"			"1"
			"wide"			"640"
			"tall"			"24"
			"autoResize"	"0"
			"pinCorner"		"0"
			"visible"		"1"
			"enabled"		"1"
			"labelText"		"%movekey%"
			"textAlignment"	"North"
			"dulltext"		"0"
			"brighttext"	"0"
		}
	}
	
	"AvatarImage"
	{
		"ControlName"	"CAvatarImagePanel"
		"fieldName"		"AvatarImage"
		"xpos"			"6"
		"xpos_minmode"	"6"
		"ypos"			"6"	[$WIN32]
		"ypos"			"0"	[$X360]
		"zpos"			"99"
		"wide"			"11"
		"tall"			"11"
		"visible"		"1"
		"enabled"		"1"
		"image"			""
		"scaleImage"	"1"	
		"color_outline"	"52 48 45 255"
	}	
}




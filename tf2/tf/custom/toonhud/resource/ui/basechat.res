"Resource/UI/BaseChat.res"
{
	"HudChat"
	{
		"ControlName" 	"EditablePanel"
		"fieldName" 	"HudChat"
		"visible" 		"1"
		"enabled" 		"1"
		"xpos" 			"2" // TLC
		"xpos_minmode" 	"80" // TLC
		"ypos" 			"250" // TLC
		"ypos_minmode" 	"r114" // TLC
		"zpos" 			"9"
		"wide"			"300"
		"wide_minmode"	"268"
		"tall" 			"130"
		"tall_minmode" 	"114"
		"PaintBackgroundType" "0"
		"paintbackground"	"0"
		"paintborder"		"0"
		"border"			"NoBorder"
		"bgcolor_override"	"0 0 0 0" // Chat color
	}

	ChatInputLine
	{
		"ControlName" 			"EditablePanel"
		"fieldName" 			"ChatInputLine"
		"visible" 				"1"
		"enabled" 				"1"
		"xpos" 					"5"
		"ypos" 					"395"
		"wide"					"290"
		"wide_minmode" 			"258"
		"tall" 					"2"
		"PaintBackgroundType" 	"0"
	}

	"ChatFiltersButton"
	{
		"ControlName" 	"Button"
		"fieldName" 	"ChatFiltersButton"
		"xpos" 			"5"
		"xpos_minmode" 	"5"
		"ypos" 			"115"
		"ypos_minmode" 	"99"
		"zpos" 			"50"
		"wide" 			"15"
		"tall"			"10"
		"autoResize"	"1"
		"pinCorner"		"0"
		"visible" 		"1"
		"enabled"		"1"
		"tabPosition"	"0"
		"labelText"		""
		"textAlignment"	"center"
		"dulltext"		"0"
		"brighttext"	"0"
		"Default"		"0"
	}

	"HudChatHistory"
	{
		"ControlName" 	"RichText"
		"fieldName" 	"HudChatHistory"
		"xpos" 			"0"
		"ypos" 			"0"
		"wide"			"300"
		"wide_minmode" 	"268"
		"tall" 			"75"
		"wrap" 			"1"
		"autoResize" 	"1"
		"pinCorner" 	"1"
		"visible" 		"1"
		"enabled" 		"1"
		"labelText" 	""
		"textAlignment" "south-west"
		"font" 			"DefaultVerySmall"
		"maxchars" 		"-1"
	}
}

"Resource/UI/HudAchievementTrackerItem.res"
{	
	"HudAchievementTrackerItem"
	{
		"ControlName"	"EditablePanel"
		"fieldName"		"HudAchievementTrackerItem"
		"xpos"			"0"		[$WIN32]
		"ypos"			"0"		[$WIN32]
		"zpos"			"3"
		"wide"			"190"
		"tall"			"30"
		"visible"		"0"
		"enabled"		"1"	
		
		"PaintBackgroundType"	"2"
	}
	
	"AchievementName"
	{
		"ControlName"	"Label"
		"fieldName"		"AchievementName"
		"labeltext"		""
		"xpos"			"0"
		"ypos"			"0"
		"wide"			"190"
		"tall"			"10"
		"zpos"			"4"
		"textinsetx"	"5"
		"font"			"G_FontTiny_2"
		"textAlignment"		"north-west"
		"fgcolor_override"	"G_White"
	}
	
	"AchievementNameGlow"
	{
		"ControlName"	"Label"
		"fieldName"		"AchievementNameGlow"
		"labeltext"		""
		"xpos"			"2"
		"ypos"			"2"
		"wide"			"190"
		"tall"			"10"
		"zpos"			"4"
		"textinsetx"	"5"
		"font"			"G_FontTiny_2"
		"enabled"		"1"
		"visible"		"1"
		"textAlignment"		"north-west"
		"fgcolor_override"	"G_Shadow"
	}
	
	"AchievementDesc"
	{
		"ControlName"	"Label"
		"fieldName"		"AchievementDesc"
		"labeltext"		""
		"xpos"			"0"
		"ypos"			"10"
		"wide"			"190"
		"tall"			"18"
		"zpos"			"4"
		"textinsetx"	"5"
		"fgcolor_override"		"235 226 202 255"
		"font"			"AchievementTracker_Desc"
		"wrap"			"0"
		"TextAlignment"		"north-west"
		"fgcolor"	"G_White"
	}
	
	"ProgressBarBG"
	{
		"ControlName"		"ImagePanel"
		"fieldName"		"ProgressBarBG"
		"xpos"		"1"
		"ypos"		"0"
		"wide"		"100"
		"tall"		"3"
		"fillcolor"	 "G_LightShadow"
		"zpos"			"4"
		"visible"		"1"
		"enabled"		"1"
		"paintborder"	"1"
		"border"		"G_MeterBorder"
	}
	
	"ProgressBarBGNew"
	{
		"ControlName"		"ImagePanel"
		"fieldName"		"ProgressBarBGNew"
		"xpos"		"0"
		"ypos"		"14"
		"wide"		"100"
		"tall"		"8"
		"fillcolor"	 "G_LightShadow"
		"zpos"			"4"
		"visible"		"0"
		"enabled"		"0"
		"paintborder"	"0"
		"border"		"G_MeterBorder"
	}
	
	"ProgressBar" // current completed
	{
		"ControlName"	"ImagePanel"
		"fieldName"		"ProgressBar"
		"xpos"			"5"
		"ypos"			"0"
		"wide"			"0"
		"tall"			"4"
		"fillcolor"		"G_White"
		"zpos"			"5"
		"visible"		"1"
		"enabled"		"1"
		"paintborder"	"1"
		"border"		"G_MeterBorder"
	}
}
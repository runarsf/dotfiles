"Resource/UI/FlagStatus.res"
{	
	"Arrow"
	{
		"ControlName"	"CTFArrowPanel"
		"fieldName"		"Arrow"
		"xpos"	"50"
		"ypos"	"12"
		"zpos"			"1"
		"wide"	"60"
		"tall"	"60"
		"visible"		"1"
		"enabled"		"1"
		"scaleImage"	"1"	
		
		"if_mvm"
		{
			"xpos"			"40"
			"ypos"			"10"
			"wide"			"80"
			"tall"			"80"
		}	
	}
	"Briefcase"
	{
		"ControlName"	"CTFImagePanel"
		"fieldName"		"Briefcase"
		"xpos"	"67"
		"ypos"	"29"
		"zpos"	"2"
		"wide"	"26"
		"tall"	"26"
		"visible"		"1"
		"enabled"		"1"
		"image"			"../hud/objectives_flagpanel_briefcase"
		"scaleImage"	"1"
		
		"if_mvm"
		{
			"xpos"			"62"
			"ypos"			"31"
			"wide"			"38"
			"tall"			"38"
		}	
	}				
	"StatusIcon"
	{
		"ControlName"	"CTFImagePanel"
		"fieldName"		"StatusIcon"
		"xpos"	"70"
		"ypos"	"33"
		"zpos"	"3"
		"wide"	"22"
		"tall"	"22"
		"visible"		"1"
		"enabled"		"1"
		"image"			"../hud/objectives_flagpanel_ico_flag_home"
		"scaleImage"	"1"	
		
		"if_mvm"
		{
			"visible"	"0"
		}
	}	
}

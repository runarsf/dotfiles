"Resource/UI/MvMCreditSubPanel.res"
{	
	"HeaderLabel"
	{
		"ControlName"	"CExLabel"
		"fieldName"		"HeaderLabel"
		"font"			"G_FontSmall"
		"textinsetx"	"5"
		"labelText"		"%header%"
		"textAlignment" "west"
		"xpos"			"0"
		"ypos"			"0"
		"zpos"			"10"
		"wide"			"300"
		"tall"			"15"
		"fgcolor"		"G_White"
	}
	
	"HeaderShadowLabel"
	{
		"ControlName"	"CExLabel"
		"fieldName"		"HeaderShadowLabel"
		"font"			"G_FontSmall"
		"textinsetx"	"5"
		"labelText"		"%header%"
		"textAlignment" "west"
		"xpos"			"1"
		"ypos"			"1"
		"zpos"			"9"
		"wide"			"300"
		"tall"			"15"
		"fgcolor"		"G_Shadow"
	}
		
	"TableBackground"
	{
		"ControlName"	"EditablePanel"
		"fieldName"		"TableBackground"
		"xpos"			"0"
		"ypos"			"16"
		"zpos"			"-1"
		"wide"			"134"
		"tall"			"38"
		"visible"		"1"
		"bgcolor_override"		"20 20 20 0"
	}
	
	"CreditCollectedTextLabel"
	{
		"ControlName"	"CExLabel"
		"fieldName"		"CreditCollectedTextLabel"
		"font"			"G_FontSmall_2"
		"labelText"		"#TF_PVE_Collected"
		"textAlignment" "north-west"
		"xpos"			"7"
		"ypos"			"17"
		"wide"			"70"
		"tall"			"20"
		"fgcolor"		"G_White"
	}
	
	"CreditCollectedCountLabel"
	{
		"ControlName"	"CExLabel"
		"fieldName"		"CreditCollectedCountLabel"
		"font"			"G_FontSmall_2"
		"labelText"		"%creditscollected%"
		"textAlignment" "north-east"
		"xpos"			"80"
		"ypos"			"17"
		"wide"			"50"
		"tall"			"20"
		"fgcolor"		"CreditsGreen"
		"textinsetx"	"5"
	}
	
	"CreditMissedTextLabel"
	{
		"ControlName"	"CExLabel"
		"fieldName"		"CreditMissedTextLabel"
		"font"			"G_FontSmall_2"
		"labelText"		"#TF_PVE_Missed"
		"textAlignment" "north-west"
		"xpos"			"7"
		"ypos"			"29"
		"wide"			"70"
		"tall"			"20"
		"fgcolor"		"G_White"
	}
	
	"CreditMissedCountLabel"
	{
		"ControlName"	"CExLabel"
		"fieldName"		"CreditMissedCountLabel"
		"font"			"G_FontSmall_2"
		"labelText"		"%creditsmissed%"
		"textAlignment" "north-east"
		"xpos"			"80"
		"ypos"			"29"
		"wide"			"50"
		"tall"			"20"
		"fgcolor"		"RedSolid"
		"textinsetx"	"5"
	}
	
	"CreditBonusTextLabel"
	{
		"ControlName"	"CExLabel"
		"fieldName"		"CreditCollectedTextLabel"
		"font"			"G_FontSmall_2"
		"labelText"		"#TF_PVE_Bonus"
		"textAlignment" "north-west"
		"xpos"			"7"
		"ypos"			"41"
		"wide"			"70"
		"tall"			"20"
		"fgcolor"		"G_White"
	}
	
	"CreditBonusCountLabel"
	{
		"ControlName"	"CExLabel"
		"fieldName"		"CreditCollectedCountLabel"
		"font"			"G_FontSmall_2"
		"labelText"		"%creditbonus%"
		"textAlignment" "north-east"
		"xpos"			"80"
		"ypos"			"41"
		"wide"			"50"
		"tall"			"20"
		"fgcolor"		"CreditsGreen"
		"textinsetx"	"5"
	}
		
	"CreditRatingLabelShadow"
	{
		"ControlName"	"CExLabel"
		"fieldName"		"CreditRatingLabelShadow"
		"font"			"G_FontSmall"
		"labelText"		"%rating%"
		"textAlignment" "center"
		"xpos"			"101"
		"ypos"			"1"
		"wide"			"40"
		"tall"			"15"
		"zpos"			"-1"
		"fgcolor"		"G_Shadow"
	}
	
	"CreditRatingLabel"
	{
		"ControlName"	"CExLabel"
		"fieldName"		"CreditCollectedTextLabel"
		"font"			"G_FontSmall"
		"labelText"		"%rating%"
		"textAlignment" "center"
		"xpos"			"100"
		"ypos"			"0"
		"wide"			"40"
		"tall"			"15"
		"zpos"			"-1"
		"fgcolor"		"G_Yellow"
	}
}

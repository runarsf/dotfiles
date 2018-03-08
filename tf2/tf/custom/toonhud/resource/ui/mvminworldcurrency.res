"Resource/UI/MvMInWorldCurrency.res"
{
	"BorderBG"
	{
		"ControlName"	"EditablePanel"
		"fieldName"		"BorderBG"
		"xpos"			"0"
		"ypos"			"0"
		"zpos"			"1"
		"wide"			"46"
		"tall"			"14"
		"visible"		"0"
		"enabled"		"0"
		"PaintBackgroundType"	"0"
		"bgcolor_override"	"TanLight"
	}	
	
	"BackgroundGood"
	{
		"ControlName"	"EditablePanel"
		"fieldName"		"BackgroundGood"
		"xpos"			"0"
		"ypos"			"0"
		"zpos"			"2"
		"wide"			"50"
		"tall"			"20"
		"visible"		"0"
		"enabled"		"0"
		"Alpha"			"0"
		"PaintBackgroundType"	"0"
		"bgcolor_override"	"G_BrownBg" //"221 182 72 250"
		"paintborder"		"1"
		"border"			"G_TargetBorder"
	}
	
	"MoneyImagePanel"
	{
		"ControlName"	"ImagePanel"
		"fieldName"		"MoneyImagePanel"
		"xpos"		"0"
		"ypos"		"5"
		"zpos"		"4"
		"wide"		"16"
		"tall"		"16"
		"image"			"../HUD/mvm_cash"
		"visible"		"0"
		"enabled"		"0"
		"scaleImage"	"1"
	}
	
	"CurrencyGood"
	{
		"ControlName"	"CExLabel"
		"fieldName"		"CurrencyGood"
		"font"			"G_FontMediumBlur"
		"fgcolor"		"G_Black"
		"xpos"			"c-90"
		"ypos"			"r135"
		"ypos_minmode"	"r135" // 1 CIM
		"zpos"			"4"
		"wide"			"180"
		"tall"			"20"
		"visible"		"1"
		"enabled"		"1"
		"textAlignment"	"center"
		"labelText"		"%currency%"
	}
	
	"CurrencyBad"
	{
		"ControlName"	"CExLabel"
		"fieldName"		"CurrencyBad"
		"font"			"G_FontMediumBlur"
		"fgcolor"		"G_CurrencyBad"
		"xpos"			"c-90"
		"ypos"			"r135"
		"ypos_minmode"	"r135" // 1 CIM
		"zpos"			"4"
		"wide"			"180"
		"tall"			"20"
		"visible"		"1"
		"enabled"		"1"
		"textAlignment"	"center"
		"labelText"		"%currency%"
		"textinsetx"	"1"
	}
	
	"Currency"
	{
		"ControlName"	"CExLabel"
		"fieldName"		"Currency"
		"font"			"G_FontMedium"
		"fgcolor"		"G_White"
		"xpos"			"c-90"
		"ypos"			"r135"
		"ypos_minmode"	"r135" // 1 CIM
		"zpos"			"10"
		"wide"			"180"
		"tall"			"20"
		"visible"		"1"
		"enabled"		"1"
		"textAlignment"	"center"
		"labelText"		"%currency%"
	}
	
	"CurrencyShadowBlur"
	{
		"ControlName"	"CExLabel"
		"fieldName"		"CurrencyShadowBlur"
		"font"			"G_FontMediumBlur"
		"fgcolor"		"G_Black"
		"xpos"			"c-90"
		"ypos"			"r135"
		"ypos_minmode"	"r135" // 1 CIM
		"zpos"			"4"
		"wide"			"180"
		"tall"			"20"
		"visible"		"1"
		"enabled"		"1"
		"textAlignment"	"center"
		"labelText"		"%currency%"
	}
	
	"CurrencyShadow"
	{
		"ControlName"	"CExLabel"
		"fieldName"		"CurrencyShadow"
		"font"			"G_FontMedium"
		"fgcolor"		"G_Shadow"
		"xpos"			"c-89"
		"ypos"			"r134"
		"ypos_minmode"	"r134" // 2 CIM
		"zpos"			"4"
		"wide"			"180"
		"tall"			"20"
		"visible"		"1"
		"enabled"		"1"
		"textAlignment"	"center"
		"labelText"		"%currency%"
	}
}
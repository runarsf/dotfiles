"Resource/UI/WaveCompletePanel.res"
{		
	"Background"
	{
		"ControlName"	"EditablePanel"
		"fieldName"		"Background"
		"xpos"			"c-77"
		"ypos"			"r75"
		"wide"			"154"
		"tall"			"70"
		"autoResize"	"0"
		"pinCorner"		"0"
		"visible"		"1"
		"enabled"		"1"
		"paintbackground"	"1"
		"bgcolor_override"	"G_PanelBg"
	}
	
	"Borders"
	{
		"ControlName"	"EditablePanel"
		"fieldName"		"Borders"
		"xpos"			"c-77"
		"ypos"			"r75"
		"zpos"			"100"
		"wide"			"154"
		"tall"			"70"
		"visible"		"1"
		"enabled"		"1"
		"border"		"G_TargetBorder"		
	}
		
	"WaveCompleteContainer"
	{
		"ControlName"	"EditablePanel"
		"fieldName"		"WaveCompleteContainer"
		"xpos"			"c-77"
		"ypos"			"r75"
		"wide"			"154"
		"tall"			"20"
		"visible"		"1"
		"PaintBackgroundType" "0"
		
		"RedPanel" 
		{
			"ControlName"	"Panel"
			"fieldName"		"RedPanel"
			"paintbackground"	"1"
			"bgcolor_override"	"190 60 60 225"
			"xpos"			"0"
			"ypos"			"0"
			"zpos"			"0"
			"wide"			"154"
			"tall"			"20"
			"visible"		"1"
			"enabled"		"1"
		}
		
		"Gradient"
		{
			"ControlName"		"CTFImagePanel"
			"fieldName"			"Gradient"
			"xpos"				"0"
			"ypos"				"20"
			"zpos"				"1"
			"wide"				"154"
			"tall"				"20"
			"visible"			"1"
			"enabled"			"1"
			"image"				"replay/thumbnails/gradient"
			"scaleImage"		"1"
		}
		
		"WaveCompleteLabelShadow"
		{
			"ControlName"	"CExLabel"
			"fieldName"		"WaveCompleteLabelShadow"
			"font"			"G_FontSmall"
			"textAlignment"	"center"
			"labelText"		"%titletext%"
			"xpos"			"1"
			"ypos"			"1"
			"zpos"			"4"
			"wide"			"154"
			"tall"			"20"
			"visible"		"1"
			"enabled"		"1"
			"fgcolor"		"G_Shadow"		
		}
		
		"WaveCompleteLabel"
		{
			"ControlName"	"CExLabel"
			"fieldName"		"WaveCompleteLabel"
			"font"			"G_FontSmall"
			"textAlignment"	"center"
			"labelText"		"%titletext%"
			"xpos"			"0"
			"ypos"			"0"
			"zpos"			"5"
			"wide"			"154"
			"tall"			"20"
			"visible"		"1"
			"enabled"		"1"
			"fgcolor"		"G_White"		
		}
	}
		
	"CreditContainer"
	{
		"ControlName"	"EditablePanel"
		"fieldName"		"CreditContainer"
		"xpos"			"c-77"
		"ypos"			"r55"
		"wide"			"154"
		"tall"			"200"
		"autoResize"	"1"
		"visible"		"1"
		
		"CreditCollectedTextLabel"
		{
			"ControlName"	"CExLabel"
			"fieldName"		"CreditCollectedTextLabel"
			"font"			"G_FontSmall_2"
			"labelText"		"#TF_PVE_CurrencyCollected"
			"textAlignment" "east"
			"xpos"			"0"
			"ypos"			"0"
			"wide"			"85"
			"tall"			"16"
			"fgcolor"		"G_White"
		}
		
		"CreditCollectedCountLabel"
		{
			"ControlName"	"CExLabel"
			"fieldName"		"CreditCollectedCountLabel"
			"font"			"G_FontSmall_2"
			"labelText"		"%creditscollected%"
			"textAlignment" "west"
			"xpos"			"95"
			"ypos"			"0"
			"wide"			"30"
			"tall"			"16"
			"fgcolor"		"CreditsGreen"
		}
		
		"CreditMissedTextLabel"
		{
			"ControlName"	"CExLabel"
			"fieldName"		"CreditMissedTextLabel"
			"font"			"G_FontSmall_2"
			"labelText"		"#TF_PVE_Missed"
			"textAlignment" "east"
			"xpos"			"0"
			"ypos"			"16"
			"wide"			"85"
			"tall"			"16"
			"fgcolor"		"G_White"
		}
		
		"CreditMissedCountLabel"
		{
			"ControlName"	"CExLabel"
			"fieldName"		"CreditMissedCountLabel"
			"font"			"G_FontSmall_2"
			"labelText"		"%creditsmissed%"
			"textAlignment" "west"
			"xpos"			"95"
			"ypos"			"16"
			"wide"			"30"
			"tall"			"16"
			"fgcolor"		"RedSolid"
		}
		
		"CreditBonusTextLabel"
		{
			"ControlName"	"CExLabel"
			"fieldName"		"CreditBonusTextLabel"
			"font"			"G_FontSmall_2"
			"labelText"		"#TF_PVE_Bonus"
			"textAlignment" "east"
			"xpos"			"0"
			"ypos"			"32"
			"wide"			"85"
			"tall"			"16"
			"fgcolor"		"G_White"
		}
		
		"CreditBonusCountLabel"
		{
			"ControlName"	"CExLabel"
			"fieldName"		"CreditBonusCountLabel"
			"font"			"G_FontSmall_2"
			"labelText"		"%creditbonus%"
			"textAlignment" "west"
			"xpos"			"95"
			"ypos"			"32"
			"wide"			"30"
			"tall"			"16"
			"fgcolor"		"CreditsGreen"
		}
	}
	
	"RatingContainer"
	{
		"ControlName"	"EditablePanel"
		"fieldName"		"RatingContainer"
		"xpos"			"c-77"
		"ypos"			"r105"
		"wide"			"154"
		"tall"			"30"
		"autoResize"	"1"
		"visible"		"1"
		
		"RatingLabel"
		{
			"ControlName"	"CExLabel"
			"fieldName"		"RatingLabel"
			"font"			"G_FontMedium_2"
			"labelText"		"%ratinglabel%"
			"textAlignment" "center"
			"xpos"			"0"
			"ypos"			"13"
			"wide"			"105"
			"tall"			"20"
			"fgcolor"		"G_White"
			"visible"		"0"
			"enabled"		"0"
		}
		
		"RatingTextShadow"
		{
			"ControlName"	"CExLabel"
			"fieldName"		"RatingTextShadow"
			"font"			"G_FontBig"
			"labelText"		"%ratingscore%"
			"textAlignment" "center"
			"textinsetx"	"5"
			"xpos"			"5"
			"ypos"			"2"
			"wide"			"154"
			"tall"			"30"
			"fgcolor"		"G_Shadow"
		}
		
		"RatingText"
		{
			"ControlName"	"CExLabel"
			"fieldName"		"RatingText"
			"font"			"G_FontBig"
			"labelText"		"%ratingscore%"
			"textAlignment" "center"
			"textinsetx"	"5"
			"xpos"			"3"
			"ypos"			"0"
			"wide"			"154"
			"tall"			"30"
			"fgcolor"		"G_White"
		}
	}
	
	"RespecBackground"
	{
		"ControlName"	"ScalableImagePanel"
		"fieldName"		"RespecBackground"
		"xpos"			"-9999"
		"ypos"			"-9999"
		"wide"			"154"
		"tall"			"12"
		"autoResize"	"0"
		"pinCorner"		"0"
		"visible"		"0"
		"enabled"		"0"
		"image"			"../HUD/tournament_panel_brown"
		"paintborder"	"1"
		"border"		"G_TargetBorder"
		"src_corner_height"	"22"				// pixels inside the image
		"src_corner_width"	"22"
		"draw_corner_width"	"0"				// screen size of the corners ( and sides ), proportional
		"draw_corner_height" 	"0"	
		"Alpha"					"0"
	}

	"RespecContainer"
	{
		"ControlName"	"EditablePanel"
		"fieldName"		"RespecContainer"
		"xpos"			"c-77"
		"ypos"			"c-40"
		"wide"			"154"
		"tall"			"12"
		"visible"		"1"
		"PaintBackgroundType" "2"
	
		"RespecTextLabelWin"
		{
			"ControlName"	"CExLabel"
			"fieldName"		"RespecTextLabelWin"
			"font"			"G_FontSmall_2"
			"labelText"		"#TF_PVE_Respecs"
			"textAlignment" "east"
			"xpos"			"0"
			"ypos"			"0"
			"wide"			"112"
			"tall"			"12"
			"fgcolor"		"G_White"
		}
		
		"RespecCountLabel"
		{
			"ControlName"	"CExLabel"
			"fieldName"		"RespecCountLabel"
			"font"			"G_FontSmall_2"
			"labelText"		"%respeccount%"
			"textAlignment" "west"
			"xpos"			"117"
			"ypos"			"0"
			"wide"			"37"
			"tall"			"12"
			"fgcolor"		"CreditsGreen"
		}
	}
}

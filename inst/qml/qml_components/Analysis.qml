//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//
import QtQuick			2.8
import QtQuick.Layouts	1.3
import JASP.Controls	1.0
import JASP.Widgets		1.0
import JASP				1.0

Section
{
	property bool directionalTest:	true

	expanded:	true
	title:		qsTr("Analysis")

	Group
	{
		RadioButtonGroup
		{
			name:		"alternativeHypothesis"
			title:		qsTr("Alternative Hypothesis")
			visible:	directionalTest
			RadioButton { value: "equal";		label: qsTr("Group 1 ≠ Group 2"); checked: true	}
			RadioButton { value: "greater";		label: qsTr("Group 1 > Group 2")}
			RadioButton { value: "less";		label: qsTr("Group 1 < Group 2")}
		}

		BayesFactorType { }

		CheckBox
		{
			name:	"bayesFactorAtOmega"	
			label:	qsTr("Bayes factor at ω")
			childrenOnSameRow:	true

			DoubleField
			{
				name:			"bayesFactorAtOmegaValue"
				defaultValue:	1
			}
		}
	}


	Group
	{
		title: qsTr("Plots")
		Layout.rowSpan: 2

		CheckBox
		{
			checked:	true 
			name:		"plotBayesFactorFunction"
			label:		qsTr("Bayes factor function")

			CheckBox
			{
				name:		"plotBayesFactorFunctionAdditionalInfo"
				label:		qsTr("Additional info")
				checked:	true
			}
		}

		CheckBox
		{
			name:		"plotPriorAndPosterior"
			label:		qsTr("Prior and posterior")

			CheckBox
			{
				name:		"plotPriorAndPosteriorAdditionalInfo"
				label:		qsTr("Additional info")
				checked:	true
			}
		}
	}
}
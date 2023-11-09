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

import QtQuick
import QtQuick.Layouts
import JASP
import JASP.Controls

Form {
	id: form
	property int framework:	Common.Type.Framework.Bayesian

	plotHeight: 340
	plotWidth:  420

	VariablesForm
	{
		preferredHeight: jaspTheme.smallDefaultVariablesFormHeight

		AvailableVariablesList
		{
			name: "allVariablesList"
		}

		AssignedVariablesList {
			name:				"tStatistic"
			title:				qsTr("T-Statistic")
			allowedColumns:		["scale"]
			singleVariable:		true
		}

		AssignedVariablesList
		{
			name:				"sampleSizeGroup1"
			title:				qsTr("Sample Size Group 1")
			allowedColumns:		["ordinal", "scale"]
			singleVariable:		true
		}

		AssignedVariablesList
		{
			name:				"sampleSizeGroup2"
			title:				qsTr("Sample Size Group 2")
			allowedColumns:		["ordinal", "scale"]
			singleVariable:		true
		}
	}

	RadioButtonGroup
	{
		name:	"alternativeHypothesis"
		title:	qsTr("Alternative Hypothesis")
		RadioButton { value: "equal";		label: qsTr("Group 1 ≠ Group 2"); checked: true	}
		RadioButton { value: "greater";		label: qsTr("Group 1 > Group 2")}
		RadioButton { value: "less";		label: qsTr("Group 1 < Group 2")}
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

	Divider { }

	RadioButtonGroup
	{
		name:	"priorR"
		title:	qsTr("Prior r")

		RadioButton
		{
			value:		"automatic"
			label:		qsTr("Automatic")
			checked:	true
		}
		RadioButton
		{
			value:		"manual"
			label:		qsTr("Manual")
			childrenOnSameRow:	true

			DoubleField
			{
				name:			"priorRManualValue"
				defaultValue:	1
				min:			1
				inclusive:		JASP.MinOnly
			}
		}
	}
}

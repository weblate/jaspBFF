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
import "../qml/qml_components" as BFF

Form {
	id: form
	property int framework:	Common.Type.Framework.Bayesian

	plotHeight: 340
	plotWidth:  420

	Group
	{
		DoubleField 
		{
			name:				"tStatistic";
			label:				qsTr("T-statistic")
			negativeValues:		true
		}

		IntegerField
		{
			name:				"sampleSizeGroup1"
			label:				qsTr("Sample size group 1")
		}
		
		IntegerField
		{
			name:				"sampleSizeGroup2"
			label:				 qsTr("Sample size group 2")
		}
	}
	
	/*VariablesForm
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
	}*/

	BFF.Analysis{}
	BFF.Priors{}
}

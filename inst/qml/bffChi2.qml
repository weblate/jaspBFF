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
			name:				"chi2Statistic";
			label:				qsTr("Chi² statistic")
			negativeValues:		true
		}

		IntegerField
		{
			name:				"sampleSize"
			label:				qsTr("Sample size")
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
			name:				"chi2Statistic"
			title:				qsTr("Chi2 Statistic")
			allowedColumns:		["scale"]
			singleVariable:		true
		}

		AssignedVariablesList
		{
			name:				"degreesOfFreedom"
			title:				qsTr("Degrees of Freedom")
			allowedColumns:		["ordinal", "scale"]
			singleVariable:		true
		}
	}*/

	BFF.Analysis{directionalTest:	false}
	BFF.Priors{}
}

import QtQuick		2.12
import JASP.Module	1.0

Description
{
	name		: "jaspBFF"
	title		: qsTr("BFF")
	description	: qsTr("This module offers Bayes factor functions.")
	version		: "0.18"
	author		: "JASP Team"
	maintainer	: "JASP Team <info@jasp-stats.org>"
	website		: "https://jasp-stats.org"
	license		: "GPL (>= 2)"
	icon		: "analysis-classical-ttest.svg"

	Analysis
	{
		title:	qsTr("Independent Samples T-Test")
		func:	"bffIndependentSamplesTTest"
	}
}

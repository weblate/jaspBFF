import QtQuick		2.12
import JASP.Module	1.0

Description
{
	name		: "jaspBFF"
	title		: qsTr("BFF (beta)")
	description	: qsTr("This module offers Bayes factor functions.")
	version			: "0.19.2"
	author		: "JASP Team"
	maintainer	: "JASP Team <info@jasp-stats.org>"
	website		: "https://jasp-stats.org"
	license		: "GPL (>= 2)"
	icon		: "analysis-bayes-factor-functions.svg"
	requiresData: false

	GroupTitle
	{
		title:	qsTr("Z-Tests")
		icon:	"analysis-classical-ttest.svg"
	}

	Analysis
	{
		title:	qsTr("One Sample Z-Test")
		func:	"bffOneSampleZTest"
	}

	Analysis
	{
		title:	qsTr("Independent Samples Z-Test")
		func:	"bffIndependentSamplesZTest"
	}

	GroupTitle
	{
		title:	qsTr("T-Tests")
		icon:	"analysis-bayesian-ttest.svg"
	}

	Analysis
	{
		title:	qsTr("One Sample T-Test")
		func:	"bffOneSampleTTest"
	}

	Analysis
	{
		title:	qsTr("Independent Samples T-Test")
		func:	"bffIndependentSamplesTTest"
	}

	GroupTitle
	{
		title:	qsTr("Regression")
		icon:	"analysis-bayesian-regression.svg"
	}

	/*Analysis
	{
		title:	qsTr("Correlation")
		func:	"bffCorrelation"
	}*/

	Analysis
	{
		title:	qsTr("Regression")
		func:	"bffRegression"
	}

	Analysis
	{
		title:	qsTr("ANOVA")
		func:	"bffANOVA"
	}

	GroupTitle
	{
		title: qsTr("Frequencies")
		icon: "analysis-bayesian-crosstabs.svg"
	}

	/*Analysis
	{
		title:	qsTr("Binomial test")
		func:	"bffBinomialTest"
	}*/

	Analysis
	{
		title:	qsTr("Chi² Test")
		func:	"bffChi2"
	}
}

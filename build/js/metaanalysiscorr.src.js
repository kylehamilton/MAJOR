
// This file is an automatically generated and should not be edited

'use strict';

const options = [{"name":"data","type":"Data"},{"name":"rcor","title":"Correlations","suggested":["continuous"],"permitted":["numeric"],"type":"Variable"},{"name":"samplesize","title":"Sample sizes","suggested":["continuous"],"permitted":["numeric"],"type":"Variable"},{"name":"slab","title":"Study label","suggested":["nominal"],"type":"Variable"},{"name":"moderatorcor","title":"Moderator","suggested":["continuous"],"permitted":["numeric"],"type":"Variable"},{"name":"methodmetacor","title":"Model estimator","type":"List","options":[{"name":"DL","title":"DerSimonian-Laird"},{"name":"HE","title":"Hedges"},{"name":"HS","title":"Hunter-Schmidt"},{"name":"SJ","title":"Sidik-Jonkman"},{"name":"ML","title":"Maximum-Likelihood"},{"name":"REML","title":"Restricted Maximum-Likelihood"},{"name":"EB","title":"Empirical Bayes"},{"name":"PM","title":"Paule-Mandel"},{"name":"FE","title":"Fixed-Effect"}],"default":"REML"},{"name":"cormeasure","title":"Model measures","type":"List","options":[{"name":"COR","title":"Raw correlation coefficient"},{"name":"UCOR","title":"Raw correlation coefficient (bias corrected)"},{"name":"ZCOR","title":"Fisher's r-to-z transformed correlation coefficient"}],"default":"ZCOR"},{"name":"moderatorType","title":"Moderator type","type":"List","options":[{"name":"NON","title":"No Moderator"},{"name":"CAT","title":"Categorical moderator"},{"name":"CON","title":"Continuous moderator"}],"default":"NON"},{"name":"testType","title":"Tests and Confidence Intervals","type":"List","options":[{"name":"z","title":"z test"},{"name":"t","title":"t test"},{"name":"knha","title":"Knapp and Hartung Adjustment"}],"default":"z"},{"name":"level","type":"Number","title":"Confidence interval level","min":50,"max":99.9,"default":95},{"name":"showModelFit","title":"Display model fit","type":"Bool","default":false},{"name":"addcred","title":"Prediction interval","type":"Bool","default":false},{"name":"addfit","title":"Summary estimate","type":"Bool","default":true},{"name":"showweights","title":"Model fitting weights","type":"Bool","default":false},{"name":"steps","title":"x-axis steps","type":"Number","min":1,"max":999,"default":5},{"name":"xAxisTitle","title":"x-axis title","type":"String"},{"name":"forestPlotSize","title":"Forest plot size","type":"List","options":[{"name":"SMALL","title":"Small"},{"name":"MEDIUM","title":"Medium"},{"name":"LARGE","title":"Large"},{"name":"SMALLWIDE","title":"Small Wide"},{"name":"MEDIUMWIDE","title":"Medium Wide"},{"name":"LARGEWIDE","title":"Large Wide"}],"default":"SMALL"},{"name":"funnelPlotSize","title":"Funnel plot size","type":"List","options":[{"name":"SMALL","title":"Small"},{"name":"MEDIUM","title":"Medium"},{"name":"LARGE","title":"Large"}],"default":"SMALL"},{"name":"pchForest","title":"Effect size points","type":"List","options":[{"name":"16","title":"Black Circle"},{"name":"18","title":"Black Diamond"},{"name":"15","title":"Black Square"},{"name":"17","title":"Black Triangle"},{"name":"1","title":"Empty Circle"},{"name":"5","title":"Empty Diamond"},{"name":"0","title":"Empty Square"},{"name":"2","title":"Empty Triangle"},{"name":"8","title":"Star"}],"default":"15"},{"name":"forestOrder","title":"Study order","type":"List","options":[{"name":"obs","title":"Observed effect sizes"},{"name":"fit","title":"Fitted values"},{"name":"prec","title":"Sampling variances"},{"name":"resid","title":"Residuals"},{"name":"abs.resid","title":"Absolute residuals"}],"default":"fit"},{"name":"fsntype","title":"Fail-Safe N method","type":"List","options":[{"name":"Rosenthal","title":"Rosenthal"},{"name":"Orwin","title":"Orwin"},{"name":"Rosenberg","title":"Rosenberg"}],"default":"Rosenthal"},{"name":"tesAlternative","title":"Hypothesis test type","type":"List","options":[{"name":"two.sided","title":"Two Sided"},{"name":"less","title":"Less"},{"name":"greater","title":"Greater"}],"default":"two.sided"},{"name":"tesAlpha","type":"Number","title":"Alpha level","min":0.000001,"max":1,"default":0.5},{"name":"tesH0","type":"Number","title":"Effect size under the null hypothesis","min":-9999,"max":9999,"default":0},{"name":"showTes","title":"Show Test of Excess Significance output","type":"Bool","default":false},{"name":"puniformSide","title":"Effect size direction","type":"List","options":[{"name":"right","title":"Right"},{"name":"left","title":"Left"}],"default":"right"},{"name":"selModelType","title":"Selection model type","type":"List","options":[{"name":"none","title":"None"},{"name":"beta","title":"Beta"},{"name":"halfnorm","title":"Half normal"},{"name":"negexp","title":"Negative exponential"},{"name":"logistic","title":"Logistic"},{"name":"power","title":"Power"},{"name":"stepfun","title":"Step function"}],"default":"none"},{"name":"yaxis","title":"Funnel plot y-axis options","type":"List","options":[{"name":"sei","title":"Standard error"},{"name":"vi","title":"Sampling variance"},{"name":"ni","title":"Sample size"},{"name":"sqrtni","title":"Square root sample size"},{"name":"lni","title":"Log of the sample size"}],"default":"sei"},{"name":"yaxisInv","title":"Funnel plot y-axis inverse","type":"Bool","default":false},{"name":"enhanceFunnel","title":"Contour-Enhanced","type":"Bool","default":false},{"name":"showInfPlot","title":"Show Plot of Influence Diagnostics","type":"Bool","default":false},{"name":"showLL","title":"Show Likelihood Plot","type":"Bool","default":false},{"name":"showPuniform","title":"Show p-uniform output","type":"Bool","default":false},{"name":"showSelmodel","title":"Show selection model output","type":"Bool","default":false},{"name":"showPcurve","title":"Show p-curve output","type":"Bool","default":false}];

const view = function() {
    
    

    View.extend({
        jus: "2.0",

        events: [

	]

    }).call(this);
}

view.layout = ui.extend({

    label: "Correlation Coefficients",
    jus: "2.0",
    type: "root",
    stage: 0, //0 - release, 1 - development, 2 - proposed
    controls: [
		{
			type: DefaultControls.VariableSupplier,
			typeName: 'VariableSupplier',
			persistentItems: false,
			stretchFactor: 1,
			controls: [
				{
					type: DefaultControls.TargetLayoutBox,
					typeName: 'TargetLayoutBox',
					label: "Correlations",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							typeName: 'VariablesListBox',
							name: "rcor",
							maxItemCount: 1,
							isTarget: true
						}
					]
				},
				{
					type: DefaultControls.TargetLayoutBox,
					typeName: 'TargetLayoutBox',
					label: "Sample Sizes",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							typeName: 'VariablesListBox',
							name: "samplesize",
							maxItemCount: 1,
							isTarget: true
						}
					]
				},
				{
					type: DefaultControls.TargetLayoutBox,
					typeName: 'TargetLayoutBox',
					label: "Moderator",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							typeName: 'VariablesListBox',
							name: "moderatorcor",
							maxItemCount: 1,
							isTarget: true
						}
					]
				},
				{
					type: DefaultControls.TargetLayoutBox,
					typeName: 'TargetLayoutBox',
					label: "Study Label",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							typeName: 'VariablesListBox',
							name: "slab",
							maxItemCount: 1,
							isTarget: true
						}
					]
				}
			]
		},
		{
			type: DefaultControls.CollapseBox,
			typeName: 'CollapseBox',
			label: "Model Options",
			collapsed: true,
			stretchFactor: 1,
			controls: [
				{
					type: DefaultControls.LayoutBox,
					typeName: 'LayoutBox',
					margin: "large",
					controls: [
						{
							type: DefaultControls.ComboBox,
							typeName: 'ComboBox',
							name: "methodmetacor",
							useSingleCell: true
						},
						{
							type: DefaultControls.ComboBox,
							typeName: 'ComboBox',
							name: "cormeasure",
							useSingleCell: true
						},
						{
							type: DefaultControls.ComboBox,
							typeName: 'ComboBox',
							name: "moderatorType",
							useSingleCell: true
						},
						{
							type: DefaultControls.ComboBox,
							typeName: 'ComboBox',
							name: "testType",
							useSingleCell: true
						},
						{
							type: DefaultControls.TextBox,
							typeName: 'TextBox',
							name: "level",
							format: FormatDef.number,
							useSingleCell: true,
							suffix: "%"
						},
						{
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							name: "showModelFit",
							useSingleCell: true
						}
					]
				}
			]
		},
		{
			type: DefaultControls.CollapseBox,
			typeName: 'CollapseBox',
			label: "Plots",
			collapsed: true,
			stretchFactor: 1,
			controls: [
				{
					type: DefaultControls.Label,
					typeName: 'Label',
					label: "Forest Plot",
					controls: [
						{
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							name: "addcred"
						},
						{
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							name: "addfit"
						},
						{
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							name: "showweights"
						},
						{
							type: DefaultControls.TextBox,
							typeName: 'TextBox',
							label: "X-Axis Steps",
							name: "steps",
							format: FormatDef.number,
							stretchFactor: 2,
							useSingleCell: true
						},
						{
							type: DefaultControls.TextBox,
							typeName: 'TextBox',
							label: "X-Axis Title",
							name: "xAxisTitle",
							format: FormatDef.string,
							stretchFactor: 2,
							useSingleCell: true
						},
						{
							type: DefaultControls.ComboBox,
							typeName: 'ComboBox',
							name: "pchForest",
							useSingleCell: true
						},
						{
							type: DefaultControls.ComboBox,
							typeName: 'ComboBox',
							name: "forestOrder",
							useSingleCell: true
						},
						{
							type: DefaultControls.ComboBox,
							typeName: 'ComboBox',
							name: "forestPlotSize",
							useSingleCell: true
						}
					]
				},
				{
					type: DefaultControls.Label,
					typeName: 'Label',
					label: "Funnel Plot",
					controls: [
						{
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							name: "yaxisInv",
							label: "Inverse",
							verticalAlignment: "center",
							enable: "(!yaxis:lni)"
						},
						{
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							name: "enhanceFunnel",
							label: "Contour-Enhanced",
							verticalAlignment: "center",
							enable: "(yaxis:sei || yaxis:vi)"
						},
						{
							type: DefaultControls.ComboBox,
							typeName: 'ComboBox',
							name: "yaxis",
							label: "Y-Axis"
						},
						{
							type: DefaultControls.ComboBox,
							typeName: 'ComboBox',
							name: "funnelPlotSize"
						}
					]
				}
			]
		},
		{
			type: DefaultControls.CollapseBox,
			typeName: 'CollapseBox',
			label: "Publication Bias",
			collapsed: true,
			stretchFactor: 1,
			controls: [
				{
					type: DefaultControls.Label,
					typeName: 'Label',
					label: "Fail Safe N",
					controls: [
						{
							type: DefaultControls.ComboBox,
							typeName: 'ComboBox',
							name: "fsntype"
						}
					]
				},
				{
					type: DefaultControls.Label,
					typeName: 'Label',
					label: "Test of Excess Significance",
					controls: [
						{
							type: DefaultControls.ComboBox,
							typeName: 'ComboBox',
							name: "tesAlternative"
						},
						{
							type: DefaultControls.TextBox,
							typeName: 'TextBox',
							name: "tesAlpha",
							format: FormatDef.number
						},
						{
							type: DefaultControls.TextBox,
							typeName: 'TextBox',
							name: "tesH0",
							format: FormatDef.number
						},
						{
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							name: "showTes"
						}
					]
				},
				{
					type: DefaultControls.Label,
					typeName: 'Label',
					label: "p-curve",
					controls: [
						{
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							name: "showPcurve"
						}
					]
				},
				{
					type: DefaultControls.Label,
					typeName: 'Label',
					label: "p-uniform",
					controls: [
						{
							type: DefaultControls.ComboBox,
							typeName: 'ComboBox',
							name: "puniformSide"
						},
						{
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							name: "showPuniform"
						}
					]
				},
				{
					type: DefaultControls.Label,
					typeName: 'Label',
					label: "Selection Models",
					controls: [
						{
							type: DefaultControls.ComboBox,
							typeName: 'ComboBox',
							name: "selModelType"
						},
						{
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							name: "showSelmodel"
						}
					]
				}
			]
		},
		{
			type: DefaultControls.CollapseBox,
			typeName: 'CollapseBox',
			label: "Additional Options",
			collapsed: true,
			stretchFactor: 1,
			controls: [
				{
					type: DefaultControls.LayoutBox,
					typeName: 'LayoutBox',
					controls: [
						{
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							name: "showInfPlot"
						},
						{
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							name: "showLL"
						}
					]
				}
			]
		}
	]
});

module.exports = { view : view, options: options };

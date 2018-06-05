
// This file is an automatically generated and should not be edited

'use strict';

const options = [{"name":"data","type":"Data"},{"name":"rcor","title":"Correlations","suggested":["continuous"],"permitted":["continuous"],"type":"Variable"},{"name":"samplesize","title":"Sample sizes","suggested":["continuous"],"permitted":["continuous"],"type":"Variable"},{"name":"slab","title":"Study label","suggested":["nominaltext"],"type":"Variable"},{"name":"cormeasure","title":"Model measures","type":"List","options":[{"name":"COR","title":"Raw correlation coefficient"},{"name":"UCOR","title":"Raw correlation coefficient (bias corrected)"},{"name":"ZCOR","title":"Fisher's r-to-z transformed correlation coefficient"}],"default":"ZCOR"},{"name":"tauPrior","title":"Heterogeneity prior","type":"List","options":[{"name":"uniform","title":"Uniform prior"},{"name":"sqrt","title":"Square root of a uniform prior"},{"name":"halfCauchy","title":"Half Cauchy prior"}],"default":"uniform"},{"name":"scalePrior","title":"Scale parameter","type":"Number","min":0.01,"max":100,"default":10},{"name":"muPrior","title":"Effect prior","type":"List","options":[{"name":"uniform","title":"Uniform prior"},{"name":"normal","title":"Normal prior distribution"}],"default":"uniform"},{"name":"muMeanPrior","title":"Mean effect","type":"Number","min":0.01,"max":100,"default":0.5},{"name":"muStandardDeviationPrior","title":"Standard deviation prior","type":"Number","min":0.01,"max":100,"default":0.2}];

const view = View.extend({
    jus: "2.0",

    events: [

	]

});

view.layout = ui.extend({

    label: "Correlation Coefficients",
    jus: "2.0",
    type: "root",
    stage: 0, //0 - release, 1 - development, 2 - proposed
    controls: [
		{
			type: DefaultControls.VariableSupplier,
			persistentItems: false,
			stretchFactor: 1,
			controls: [
				{
					type: DefaultControls.TargetLayoutBox,
					label: "Correlations",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							name: "rcor",
							maxItemCount: 1,
							isTarget: true
						}
					]
				},
				{
					type: DefaultControls.TargetLayoutBox,
					label: "Sample Sizes",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							name: "samplesize",
							maxItemCount: 1,
							isTarget: true
						}
					]
				},
				{
					type: DefaultControls.TargetLayoutBox,
					label: "Study Label",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
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
			label: "Model Options",
			collapsed: true,
			stretchFactor: 1,
			controls: [
				{
					type: DefaultControls.LayoutBox,
					margin: "large",
					controls: [
						{
							type: DefaultControls.ComboBox,
							name: "cormeasure",
							useSingleCell: true
						}
					]
				}
			]
		},
		{
			type: DefaultControls.CollapseBox,
			label: "Prior Specification",
			collapsed: true,
			stretchFactor: 1,
			controls: [
				{
					type: DefaultControls.Label,
					label: "Tau Prior Specification",
					controls: [
						{
							type: DefaultControls.ComboBox,
							name: "tauPrior",
							useSingleCell: true
						},
						{
							type: DefaultControls.TextBox,
							label: "Scale parameter",
							name: "scalePrior",
							format: FormatDef.number,
							stretchFactor: 2,
							useSingleCell: true
						}
					]
				},
				{
					type: DefaultControls.Label,
					label: "Effect Prior Specification",
					controls: [
						{
							type: DefaultControls.ComboBox,
							name: "muPrior"
						},
						{
							type: DefaultControls.TextBox,
							name: "muMeanPrior",
							format: FormatDef.number,
							inputPattern: "[0-9]+"
						},
						{
							type: DefaultControls.TextBox,
							name: "muStandardDeviationPrior",
							format: FormatDef.number,
							inputPattern: "[0-9]+"
						}
					]
				}
			]
		}
	]
});

module.exports = { view : view, options: options };

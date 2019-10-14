
// This file is an automatically generated and should not be edited

'use strict';

const options = [{"name":"data","type":"Data"},{"name":"rcor","title":"Correlations","suggested":["continuous"],"permitted":["numeric"],"type":"Variable"},{"name":"samplesize","title":"Sample sizes","suggested":["continuous"],"permitted":["numeric"],"type":"Variable"},{"name":"slab","title":"Study label","suggested":["nominal"],"type":"Variable"},{"name":"cormeasure","title":"Model measures","type":"List","options":[{"name":"COR","title":"Raw correlation coefficient"},{"name":"UCOR","title":"Raw correlation coefficient (bias corrected)"},{"name":"ZCOR","title":"Fisher's r-to-z transformed correlation coefficient"}],"default":"ZCOR"},{"name":"tauPrior","title":"Heterogeneity prior","type":"List","options":[{"name":"halfCauchy","title":"Half Cauchy"},{"name":"halfNormal","title":"Half Normal"},{"name":"uniform","title":"Uniform prior"},{"name":"sqrt","title":"Square root of a uniform prior"},{"name":"normal","title":"Normal prior"},{"name":"Jeffreys","title":"Jeffreys prior"},{"name":"conventional","title":"Conventional prior"},{"name":"DuMouchel","title":"DuMouchel prior"},{"name":"shrinkage","title":"Uniform shrinkage prior"},{"name":"I2","title":"Relative heterogeneity uniform prior"}],"default":"uniform"},{"name":"scalePrior","title":"Scale parameter","type":"Number","min":0.01,"max":100,"default":10},{"name":"muPrior","title":"Effect prior","type":"List","options":[{"name":"uniform","title":"Uniform prior"},{"name":"normal","title":"Normal prior"}],"default":"uniform"},{"name":"muMeanPrior","title":"Mean effect","type":"Number","min":0.01,"max":100,"default":0.5},{"name":"muStandardDeviationPrior","title":"Standard deviation prior","type":"Number","min":0.01,"max":100,"default":0.2}];

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
							name: "cormeasure",
							useSingleCell: true
						}
					]
				}
			]
		},
		{
			type: DefaultControls.CollapseBox,
			typeName: 'CollapseBox',
			label: "Prior Specification",
			collapsed: true,
			stretchFactor: 1,
			controls: [
				{
					type: DefaultControls.Label,
					typeName: 'Label',
					label: "Tau Prior Specification",
					controls: [
						{
							type: DefaultControls.ComboBox,
							typeName: 'ComboBox',
							name: "tauPrior",
							useSingleCell: true
						},
						{
							type: DefaultControls.TextBox,
							typeName: 'TextBox',
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
					typeName: 'Label',
					label: "Effect Prior Specification",
					controls: [
						{
							type: DefaultControls.ComboBox,
							typeName: 'ComboBox',
							name: "muPrior"
						},
						{
							type: DefaultControls.TextBox,
							typeName: 'TextBox',
							name: "muMeanPrior",
							format: FormatDef.number
						},
						{
							type: DefaultControls.TextBox,
							typeName: 'TextBox',
							name: "muStandardDeviationPrior",
							format: FormatDef.number
						}
					]
				}
			]
		}
	]
});

module.exports = { view : view, options: options };

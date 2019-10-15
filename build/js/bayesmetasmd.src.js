
// This file is an automatically generated and should not be edited

'use strict';

const options = [{"name":"data","type":"Data"},{"name":"n1i","title":"Group one sample size","suggested":["continuous"],"permitted":["numeric"],"type":"Variable"},{"name":"m1i","title":"Group one mean","suggested":["continuous"],"permitted":["numeric"],"type":"Variable"},{"name":"sd1i","title":"Group one standard deviation","suggested":["continuous"],"permitted":["numeric"],"type":"Variable"},{"name":"n2i","title":"Group two sample size","suggested":["continuous"],"permitted":["numeric"],"type":"Variable"},{"name":"m2i","title":"Group two mean","suggested":["continuous"],"permitted":["numeric"],"type":"Variable"},{"name":"sd2i","title":"Group two standard deviation","suggested":["continuous"],"permitted":["numeric"],"type":"Variable"},{"name":"slab","title":"Study label","suggested":["nominal"],"type":"Variable"},{"name":"cormeasure","title":"Effect size model measures","type":"List","options":[{"name":"MD","title":"Raw Mean Difference"},{"name":"SMD","title":"Standardized Mean Difference"},{"name":"SMDH","title":"Standardized Mean Difference (Heteroscedastic Population Variances)"},{"name":"ROM","title":"Log Transformed Ratio of Means"}],"default":"SMD"},{"name":"tauPrior","title":"Heterogeneity prior","type":"List","options":[{"name":"halfCauchy","title":"Half Cauchy"},{"name":"halfNormal","title":"Half Normal"},{"name":"uniform","title":"Uniform prior"},{"name":"sqrt","title":"Square root of a uniform prior"},{"name":"normal","title":"Normal prior"},{"name":"Jeffreys","title":"Jeffreys prior"},{"name":"conventional","title":"Conventional prior"},{"name":"DuMouchel","title":"DuMouchel prior"},{"name":"shrinkage","title":"Uniform shrinkage prior"},{"name":"I2","title":"Relative heterogeneity uniform prior"}],"default":"uniform"},{"name":"scalePrior","title":"Scale parameter","type":"Number","min":0.01,"max":100,"default":10},{"name":"muPrior","title":"Effect prior","type":"List","options":[{"name":"uniform","title":"Uniform prior"},{"name":"normal","title":"Normal prior"}],"default":"uniform"},{"name":"muMeanPrior","title":"Mean effect","type":"Number","min":0.01,"max":100,"default":0.5},{"name":"muStandardDeviationPrior","title":"Standard deviation prior","type":"Number","min":0.01,"max":100,"default":0.2}];

const view = function() {
    
    this.handlers = { }

    View.extend({
        jus: "3.0",

        events: [

	]

    }).call(this);
}

view.layout = ui.extend({

    label: "Mean Differences",
    jus: "3.0",
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
					label: "Group one sample size",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							typeName: 'VariablesListBox',
							name: "n1i",
							maxItemCount: 1,
							isTarget: true
						}
					]
				},
				{
					type: DefaultControls.TargetLayoutBox,
					typeName: 'TargetLayoutBox',
					label: "Group one mean",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							typeName: 'VariablesListBox',
							name: "m1i",
							maxItemCount: 1,
							isTarget: true
						}
					]
				},
				{
					type: DefaultControls.TargetLayoutBox,
					typeName: 'TargetLayoutBox',
					label: "Group one standard deviation",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							typeName: 'VariablesListBox',
							name: "sd1i",
							maxItemCount: 1,
							isTarget: true
						}
					]
				},
				{
					type: DefaultControls.TargetLayoutBox,
					typeName: 'TargetLayoutBox',
					label: "Group two sample size",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							typeName: 'VariablesListBox',
							name: "n2i",
							maxItemCount: 1,
							isTarget: true
						}
					]
				},
				{
					type: DefaultControls.TargetLayoutBox,
					typeName: 'TargetLayoutBox',
					label: "Group two mean",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							typeName: 'VariablesListBox',
							name: "m2i",
							maxItemCount: 1,
							isTarget: true
						}
					]
				},
				{
					type: DefaultControls.TargetLayoutBox,
					typeName: 'TargetLayoutBox',
					label: "Group two standard deviation",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							typeName: 'VariablesListBox',
							name: "sd2i",
							maxItemCount: 1,
							isTarget: true
						}
					]
				},
				{
					type: DefaultControls.TargetLayoutBox,
					typeName: 'TargetLayoutBox',
					label: "Study label",
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
			type: DefaultControls.LayoutBox,
			typeName: 'LayoutBox',
			margin: "large",
			controls: [
				{
					type: DefaultControls.ComboBox,
					typeName: 'ComboBox',
					name: "cormeasure"
				}
			]
		},
		{
			type: DefaultControls.LayoutBox,
			typeName: 'LayoutBox',
			margin: "large",
			controls: [
				{
					type: DefaultControls.ComboBox,
					typeName: 'ComboBox',
					name: "tauPrior"
				}
			]
		},
		{
			type: DefaultControls.LayoutBox,
			typeName: 'LayoutBox',
			margin: "large",
			controls: [
				{
					type: DefaultControls.TextBox,
					typeName: 'TextBox',
					name: "scalePrior",
					format: FormatDef.number
				}
			]
		},
		{
			type: DefaultControls.LayoutBox,
			typeName: 'LayoutBox',
			margin: "large",
			controls: [
				{
					type: DefaultControls.ComboBox,
					typeName: 'ComboBox',
					name: "muPrior"
				}
			]
		},
		{
			type: DefaultControls.LayoutBox,
			typeName: 'LayoutBox',
			margin: "large",
			controls: [
				{
					type: DefaultControls.TextBox,
					typeName: 'TextBox',
					name: "muMeanPrior",
					format: FormatDef.number
				}
			]
		},
		{
			type: DefaultControls.LayoutBox,
			typeName: 'LayoutBox',
			margin: "large",
			controls: [
				{
					type: DefaultControls.TextBox,
					typeName: 'TextBox',
					name: "muStandardDeviationPrior",
					format: FormatDef.number
				}
			]
		}
	]
});

module.exports = { view : view, options: options };


// This file is an automatically generated and should not be edited

'use strict';

const options = [{"name":"data","type":"Data"},{"name":"rcor","title":"Correlations","suggested":["continuous"],"permitted":["numeric"],"type":"Variable"},{"name":"samplesize","title":"Sample sizes","suggested":["continuous"],"permitted":["numeric"],"type":"Variable"},{"name":"clusterOne","title":"Cluster Level One","permitted":["numeric"],"type":"Variable"},{"name":"clusterTwo","title":"Cluster Level Two","permitted":["numeric"],"type":"Variable"},{"name":"slab","title":"Study label","suggested":["nominal"],"type":"Variable"},{"name":"methodmetacor","title":"Model estimator","type":"List","options":[{"name":"ML","title":"Maximum-Likelihood"},{"name":"REML","title":"Restricted Maximum-Likelihood"},{"name":"FE","title":"Fixed-Effect"}],"default":"REML"},{"name":"cormeasure","title":"Model measures","type":"List","options":[{"name":"COR","title":"Raw correlation coefficient"},{"name":"UCOR","title":"Raw correlation coefficient (bias corrected)"},{"name":"ZCOR","title":"Fisher's r-to-z transformed correlation coefficient"}],"default":"ZCOR"},{"name":"showModelFit","title":"Display model fit","type":"Bool","default":false}];

const view = function() {
    
    this.handlers = { }

    View.extend({
        jus: "3.0",

        events: [

	]

    }).call(this);
}

view.layout = ui.extend({

    label: "Correlation Coefficients",
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
					label: "Sample sizes",
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
					label: "Cluster Level One",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							typeName: 'VariablesListBox',
							name: "clusterOne",
							maxItemCount: 1,
							isTarget: true
						}
					]
				},
				{
					type: DefaultControls.TargetLayoutBox,
					typeName: 'TargetLayoutBox',
					label: "Cluster Level Two",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							typeName: 'VariablesListBox',
							name: "clusterTwo",
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
					name: "methodmetacor"
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
					type: DefaultControls.CheckBox,
					typeName: 'CheckBox',
					name: "showModelFit"
				}
			]
		}
	]
});

module.exports = { view : view, options: options };

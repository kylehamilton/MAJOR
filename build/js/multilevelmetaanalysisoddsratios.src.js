
// This file is an automatically generated and should not be edited

'use strict';

const options = [{"name":"data","type":"Data"},{"name":"ai","title":"Number of positive cases in experimental group","suggested":["continuous"],"permitted":["numeric"],"type":"Variable"},{"name":"bi","title":"Number of negative cases in experimental group","suggested":["continuous"],"permitted":["numeric"],"type":"Variable"},{"name":"ci","title":"Number of negative cases in control group","suggested":["continuous"],"permitted":["numeric"],"type":"Variable"},{"name":"di","title":"Number of negative cases in control group","suggested":["continuous"],"permitted":["numeric"],"type":"Variable"},{"name":"studyNum","title":"Study or Trial Number","suggested":["continuous"],"permitted":["numeric"],"type":"Variable"}];

const view = function() {
    
    this.handlers = { }

    View.extend({
        jus: "3.0",

        events: [

	]

    }).call(this);
}

view.layout = ui.extend({

    label: "Dichotomous Models",
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
					label: "Number of positive cases in experimental group",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							typeName: 'VariablesListBox',
							name: "ai",
							maxItemCount: 1,
							isTarget: true
						}
					]
				},
				{
					type: DefaultControls.TargetLayoutBox,
					typeName: 'TargetLayoutBox',
					label: "Number of negative cases in experimental group",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							typeName: 'VariablesListBox',
							name: "bi",
							maxItemCount: 1,
							isTarget: true
						}
					]
				},
				{
					type: DefaultControls.TargetLayoutBox,
					typeName: 'TargetLayoutBox',
					label: "Number of negative cases in control group",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							typeName: 'VariablesListBox',
							name: "ci",
							maxItemCount: 1,
							isTarget: true
						}
					]
				},
				{
					type: DefaultControls.TargetLayoutBox,
					typeName: 'TargetLayoutBox',
					label: "Number of negative cases in control group",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							typeName: 'VariablesListBox',
							name: "di",
							maxItemCount: 1,
							isTarget: true
						}
					]
				},
				{
					type: DefaultControls.TargetLayoutBox,
					typeName: 'TargetLayoutBox',
					label: "Study or Trial Number",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							typeName: 'VariablesListBox',
							name: "studyNum",
							maxItemCount: 1,
							isTarget: true
						}
					]
				}
			]
		}
	]
});

module.exports = { view : view, options: options };

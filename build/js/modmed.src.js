
// This file is an automatically generated and should not be edited

'use strict';

const options = [{"name":"data","type":"Data","description":{"R":"the data as a data frame"}},{"name":"dep","title":"Dependent Variable","type":"Variable","suggested":["continuous"],"permitted":["numeric"],"description":{"R":"a string naming the dependent variable"}},{"name":"meds","title":"Mediators","type":"Variables","suggested":["continuous"],"permitted":["numeric"],"description":{"R":"a string naming the mediator variable"}},{"name":"pred","title":"Predictor","type":"Variable","suggested":["continuous"],"permitted":["numeric","factor"],"description":{"R":"a string naming the predictor variable"}},{"name":"xmmod","title":"Moderator a-path","type":"Variable","suggested":["continuous"],"permitted":["numeric","factor"],"description":{"R":"a string naming the predictor variable"}},{"name":"mymod","title":"Moderator b-path","type":"Variable","suggested":["continuous"],"permitted":["numeric","factor"],"description":{"R":"a string naming the predictor variable"}},{"name":"covsm","title":"Covariates mediators","type":"Variables","suggested":["continuous"],"permitted":["numeric"],"description":{"R":"a vector with strings naming the covariates"}},{"name":"covsy","title":"Covariates dependent","type":"Variables","suggested":["continuous"],"permitted":["numeric"],"description":{"R":"a vector with strings naming the covariates"}},{"name":"estMethod","title":"Estimation Method for SE's","type":"List","options":[{"name":"standard","title":"Standard"},{"name":"bootstrap","title":"Bootstrap"}],"default":"standard","description":{"R":"`'standard'` (default), or `'bootstrap'`, the estimation method to use\n"}},{"name":"bootstrap","title":"Samples","type":"Integer","min":1,"default":500,"description":{"R":"a number between 1 and 100000 (default: 1000) specifying the number of samples that need to been drawn in the bootstrap method\n"}},{"name":"showFit","title":"Model fit","type":"Bool","default":false,"description":{"R":"`TRUE` or `FALSE` (default), provides the lavaan fit values of the\n the mediation model\n"}},{"name":"paths","title":"Path estimates","type":"Bool","default":false,"description":{"R":"`TRUE` or `FALSE` (default), provide the individual estimates of the a and b paths in the mediation model\n"}},{"name":"plotIom","title":"Plot index of mediation","type":"Bool","default":false,"description":{"R":"`TRUE` (default) or `FALSE`, provides 'plot showing the index  of mediation for each mediator and moderator\n"}},{"name":"plotSS","title":"Plot mediated simple slopes","type":"Bool","default":false,"description":{"R":"`TRUE` or `FALSE` (default), provides a simple slope plot where for each mediator the estimated simple slopes are plotted.\n"}}];

const view = View.extend({
    jus: "2.0",

    events: [

	]

});

view.layout = ui.extend({

    label: "Moderated Mediation",
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
					label: "Dependent Variable",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							name: "dep",
							maxItemCount: 1,
							isTarget: true
						}
					]
				},
				{
					type: DefaultControls.TargetLayoutBox,
					label: "Mediators",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							name: "meds",
							isTarget: true
						}
					]
				},
				{
					type: DefaultControls.TargetLayoutBox,
					label: "Predictor",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							name: "pred",
							maxItemCount: 1,
							isTarget: true
						}
					]
				},
				{
					type: DefaultControls.TargetLayoutBox,
					label: "Moderator a-path",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							name: "xmmod",
							maxItemCount: 1,
							isTarget: true
						}
					]
				},
				{
					type: DefaultControls.TargetLayoutBox,
					label: "Moderator b-path",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							name: "mymod",
							maxItemCount: 1,
							isTarget: true
						}
					]
				},
				{
					type: DefaultControls.TargetLayoutBox,
					label: "Covariates mediators",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							name: "covsm",
							isTarget: true
						}
					]
				},
				{
					type: DefaultControls.TargetLayoutBox,
					label: "Covariates dependent",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							name: "covsy",
							isTarget: true
						}
					]
				}
			]
		},
		{
			type: DefaultControls.LayoutBox,
			margin: "large",
			controls: [
				{
					type: DefaultControls.ComboBox,
					name: "estMethod"
				}
			]
		},
		{
			type: DefaultControls.LayoutBox,
			margin: "large",
			controls: [
				{
					type: DefaultControls.TextBox,
					name: "bootstrap",
					format: FormatDef.number,
					inputPattern: "[0-9]+"
				}
			]
		},
		{
			type: DefaultControls.LayoutBox,
			margin: "large",
			controls: [
				{
					type: DefaultControls.CheckBox,
					name: "showFit"
				},
				{
					type: DefaultControls.CheckBox,
					name: "paths"
				},
				{
					type: DefaultControls.CheckBox,
					name: "plotIom"
				},
				{
					type: DefaultControls.CheckBox,
					name: "plotSS"
				}
			]
		}
	]
});

module.exports = { view : view, options: options };

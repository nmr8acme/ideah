package ideah.run;

import com.intellij.execution.configuration.EnvironmentVariablesComponent;
import com.intellij.openapi.fileChooser.FileChooserDescriptor;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.LabeledComponent;
import com.intellij.openapi.ui.TextFieldWithBrowseButton;
import com.intellij.ui.RawCommandLineEditor;

import javax.swing.*;

final class ProgramParamsPanel {

    private JPanel myContentPanel;
    private LabeledComponent<TextFieldWithBrowseButton> mainFileComponent;
    private LabeledComponent<RawCommandLineEditor> programParametersComponent;
    private LabeledComponent<TextFieldWithBrowseButton> workingDirectoryComponent;
    private LabeledComponent<JComboBox> moduleComponent;
    private EnvironmentVariablesComponent environmentVariables;
    private LabeledComponent<RawCommandLineEditor> runtimeFlagsComponent;

    ProgramParamsPanel(Project project) {
        mainFileComponent.getComponent().addBrowseFolderListener("Main file", "Main File", null,
            new FileChooserDescriptor(true, false, false, false, true, false)); // todo: chooseMultiple = false?
        workingDirectoryComponent.getComponent().addBrowseFolderListener("Working directory", "Working Directory",
            null, new FileChooserDescriptor(false, true, false, false, true, false));
        // todo: moduleComponent
    }

    public void applyTo(HaskellRunConfiguration configuration) {
        configuration.setProgramParameters(programParametersComponent.getComponent().getText());
        configuration.setWorkingDirectory(workingDirectoryComponent.getComponent().getText());
//        configuration.setModule((String) moduleComponent.getComponent().getSelectedItem()); // todo
        configuration.setRuntimeFlags(runtimeFlagsComponent.getComponent().getText());
        configuration.setEnvs(environmentVariables.getEnvs());
        configuration.setPassParentEnvs(environmentVariables.isPassParentEnvs());
    }

    public void reset(HaskellRunConfiguration configuration) {
        setProgramParameters(configuration.getProgramParameters());
        setWorkingDirectory(configuration.getWorkingDirectory());
        setMainFile(configuration.getMainFile());
        setModule(configuration.getModule());
        setRtFlags(configuration.getRuntimeFlags());
        environmentVariables.setEnvs(configuration.getEnvs());
        environmentVariables.setPassParentEnvs(configuration.isPassParentEnvs());
    }

    private void setModule(Module module) {
        moduleComponent.getComponent().setSelectedItem(module.getName());
    }

    private void setMainFile(String mainFile) {
        mainFileComponent.getComponent().setText(mainFile); // todo: system slashes
    }

    private void setWorkingDirectory(String workingDirectory) {
        workingDirectoryComponent.getComponent().setText(workingDirectory); // todo: system slashes
    }

    private void setProgramParameters(String programParameters) {
        programParametersComponent.getComponent().setText(programParameters);
    }

    private void setRtFlags(String runtimeFlags) {
        runtimeFlagsComponent.getComponent().setText(runtimeFlags);
    }

    JComponent getVisual() {
        return myContentPanel;
    }
}

package ideah.run;

import com.intellij.openapi.options.SettingsEditor;
import com.intellij.openapi.project.Project;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;

final class ConfigurationEditor extends SettingsEditor<HaskellRunConfiguration> {

    private final ProgramParamsPanel programParams;

    ConfigurationEditor(Project project) {
        programParams = new ProgramParamsPanel(project);
        // todo: selection of main module
        // todo: runtime flags
    }

    protected void applyEditorTo(HaskellRunConfiguration s) {
        programParams.applyTo(s);
    }

    protected void resetEditorFrom(HaskellRunConfiguration s) {
        programParams.reset(s);
    }

    @NotNull
    protected JComponent createEditor() {
        return programParams.getVisual();
    }

    protected void disposeEditor() {
    }
}

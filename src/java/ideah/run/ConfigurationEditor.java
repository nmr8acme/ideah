package ideah.run;

import com.intellij.openapi.module.Module;
import com.intellij.openapi.options.SettingsEditor;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;

final class ConfigurationEditor extends SettingsEditor<HaskellRunConfiguration> {

    private final ProgramParamsPanel programParams;

    ConfigurationEditor(Module[] modules, Module module) {
        programParams = new ProgramParamsPanel(modules, module);
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

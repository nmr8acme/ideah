package ideah.run;

import com.intellij.openapi.module.Module;

import javax.swing.*;
import java.awt.*;

final class ModuleComboBoxRenderer extends JLabel implements ListCellRenderer<Module> {

    public Component getListCellRendererComponent(JList<? extends Module> list, Module value, int index, boolean isSelected, boolean cellHasFocus) {
        if (value != null) {
            setText(value.getName());
        }
        return this;
    }
}

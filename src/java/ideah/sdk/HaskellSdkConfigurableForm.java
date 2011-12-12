package ideah.sdk;

import com.intellij.openapi.fileChooser.FileChooserDescriptor;
import com.intellij.openapi.ui.TextFieldWithBrowseButton;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

public final class HaskellSdkConfigurableForm {

    private JPanel myContentPanel;
    private JTextField myGhcOptionsTextField;
    private TextFieldWithBrowseButton myGhcLibPathTFWBB;
    private TextFieldWithBrowseButton myCabalTFWBB;
    private JLabel ghcLibPathLabel;
    private JLabel cabalLabel;
    private JLabel ghcOptionsLabel;

    private boolean modified = false;

    public HaskellSdkConfigurableForm() {
        myGhcLibPathTFWBB.addBrowseFolderListener("Path to GHC lib directory", "Path to GHC lib Directory",
            null, new FileChooserDescriptor(false, true, false, false, false, false));
        myCabalTFWBB.addBrowseFolderListener("Path to cabal executable", "Path to Cabal Executable",
            null, new FileChooserDescriptor(true, false, false, false, false, false));
        DocumentListener listener = new DocumentListener() {

            public void insertUpdate(DocumentEvent e) {
                modify();
            }

            public void removeUpdate(DocumentEvent e) {
                modify();
            }

            public void changedUpdate(DocumentEvent e) {
                modify();
            }
        };
        myGhcOptionsTextField.getDocument().addDocumentListener(listener);
        myGhcLibPathTFWBB.getTextField().getDocument().addDocumentListener(listener);
        myCabalTFWBB.getTextField().getDocument().addDocumentListener(listener);
    }

    private void modify() {
        modified = true;
    }

    @NotNull
    public JPanel getContentPanel() {
        return myContentPanel;
    }

    public String getLibPath() {
        return myGhcLibPathTFWBB.getText();
    }

    public String getCabalPath() {
        return myCabalTFWBB.getText();
    }

    public String getGhcOptions() {
        return myGhcOptionsTextField.getText();
    }

    public void init(String libPath, String cabalPath, String ghcOptions) {
        myGhcLibPathTFWBB.setText(libPath);
        myCabalTFWBB.setText(cabalPath);
        myGhcOptionsTextField.setText(ghcOptions);
    }

    void setModified(boolean modified) {
        this.modified = modified;
    }

    boolean isModified() {
        return modified;
    }
}

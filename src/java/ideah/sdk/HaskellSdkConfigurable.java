package ideah.sdk;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.projectRoots.AdditionalDataConfigurable;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.projectRoots.SdkAdditionalData;
import com.intellij.openapi.projectRoots.SdkModificator;

import javax.swing.*;

public final class HaskellSdkConfigurable implements AdditionalDataConfigurable {

    private final HaskellSdkConfigurableForm myForm;

    private Sdk mySdk;

    public HaskellSdkConfigurable() {
        myForm = new HaskellSdkConfigurableForm();
    }

    public void setSdk(Sdk sdk) {
        mySdk = sdk;
    }

    public JComponent createComponent() {
        return myForm.getContentPanel();
    }

    public boolean isModified() {
        HaskellSdkAdditionalData data = (HaskellSdkAdditionalData) mySdk.getSdkAdditionalData();
        return true; // todo
    }

    public void apply() {
        String libPath = myForm.getLibPath();
        String cabalPath = myForm.getCabalPath();
        String ghcOptions = myForm.getGhcOptions();
        HaskellSdkAdditionalData newData = new HaskellSdkAdditionalData(libPath, cabalPath, ghcOptions);
        final SdkModificator modificator = mySdk.getSdkModificator();
        modificator.setSdkAdditionalData(newData);
        ApplicationManager.getApplication().runWriteAction(new Runnable() {
            public void run() {
                modificator.commitChanges();
            }
        });
    }

    public void reset() {
        SdkAdditionalData data = mySdk.getSdkAdditionalData();
        if (!(data instanceof HaskellSdkAdditionalData)) {
            return;
        }
        HaskellSdkAdditionalData ghcData = (HaskellSdkAdditionalData) data;
        myForm.init(ghcData.getLibPath(), ghcData.getCabalPath(), ghcData.getGhcOptions());
    }

    public void disposeUIResources() {
    }
}

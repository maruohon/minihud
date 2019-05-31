package fi.dy.masa.minihud.hotkeys;

import fi.dy.masa.malilib.config.IConfigBoolean;
import fi.dy.masa.malilib.hotkeys.IKeybind;
import fi.dy.masa.malilib.hotkeys.KeyAction;
import fi.dy.masa.malilib.hotkeys.KeyCallbackToggleBooleanConfigWithMessage;
import fi.dy.masa.minihud.config.StructureToggle;
import fi.dy.masa.minihud.util.DataStorage;
import net.minecraft.client.Minecraft;

public class KeyCallbackToggleStructures extends KeyCallbackToggleBooleanConfigWithMessage
{
    public KeyCallbackToggleStructures(IConfigBoolean config)
    {
        super(config);
    }

    @Override
    public boolean onKeyAction(KeyAction action, IKeybind key)
    {
        Minecraft mc = Minecraft.getInstance();

        if (mc != null && mc.player != null && super.onKeyAction(action, key))
        {
            if (this.config.getBooleanValue())
            {
                StructureToggle.updateStructureData();
            }

            DataStorage.getInstance().setStructuresDirty();

            return true;
        }

        return false;
    }
}

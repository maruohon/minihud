package minihud.compat.modmenu;

import com.terraformersmc.modmenu.api.ConfigScreenFactory;
import com.terraformersmc.modmenu.api.ModMenuApi;

import malilib.gui.BaseScreen;
import minihud.gui.ConfigScreen;

public class ModMenuImpl implements ModMenuApi
{
    @Override
    public ConfigScreenFactory<?> getModConfigScreenFactory()
    {
        return (currentScreen) -> {
            BaseScreen screen = ConfigScreen.create();
            screen.setParent(currentScreen);
            return screen;
        };
    }
}

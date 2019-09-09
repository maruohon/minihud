package fi.dy.masa.itemscroller.compat.modmenu;

import java.util.function.Function;
import net.minecraft.client.gui.screen.Screen;
import fi.dy.masa.itemscroller.Reference;
import fi.dy.masa.itemscroller.gui.GuiConfigs;
import io.github.prospector.modmenu.api.ModMenuApi;

public class ModMenuImpl implements ModMenuApi
{
    @Override
    public String getModId()
    {
        return Reference.MOD_ID;
    }

    @Override
    public Function<Screen, ? extends Screen> getConfigScreenFactory()
    {
        return (screen) -> {
            GuiConfigs gui = new GuiConfigs();
            gui.setParent(screen);
            return gui;
        };
    }
}

package fi.dy.masa.minihud.compat.modmenu;

import java.util.function.Function;
import net.minecraft.client.gui.screen.Screen;
import fi.dy.masa.minihud.Reference;
import fi.dy.masa.minihud.gui.GuiConfigs;
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

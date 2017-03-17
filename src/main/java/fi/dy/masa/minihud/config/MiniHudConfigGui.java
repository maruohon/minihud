package fi.dy.masa.minihud.config;

import java.util.ArrayList;
import java.util.List;

import net.minecraft.client.gui.GuiScreen;

import net.minecraftforge.common.config.ConfigElement;
import net.minecraftforge.fml.client.config.GuiConfig;
import net.minecraftforge.fml.client.config.IConfigElement;

import fi.dy.masa.minihud.Reference;

public class MiniHudConfigGui extends GuiConfig
{
    public MiniHudConfigGui(GuiScreen parent)
    {
        super(parent, getConfigElements(), Reference.MOD_ID, false, false, getTitle(parent));
    }

    private static List<IConfigElement> getConfigElements()
    {
        List<IConfigElement> configElements = new ArrayList<IConfigElement>();

        configElements.add(new ConfigElement(Configs.config.getCategory(Configs.CATEGORY_GENERIC)));
        configElements.add(new ConfigElement(Configs.config.getCategory(Configs.CATEGORY_INFO_TOGGLE)));
        configElements.add(new ConfigElement(Configs.config.getCategory(Configs.CATEGORY_INFO_HOTKEYS)));

        return configElements;
    }

    private static String getTitle(GuiScreen parent)
    {
        return GuiConfig.getAbridgedConfigPath(Configs.configurationFile.toString());
    }
}

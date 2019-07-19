package fi.dy.masa.minihud.config.gui;

import net.minecraft.client.gui.GuiScreen;
import net.minecraftforge.fml.client.DefaultGuiFactory;
import fi.dy.masa.minihud.Reference;
import fi.dy.masa.minihud.gui.GuiConfigs;

public class MiniHudGuiFactory extends DefaultGuiFactory
{
    public MiniHudGuiFactory()
    {
        super(Reference.MOD_ID, Reference.MOD_NAME + " configs");
    }

    @Override
    public GuiScreen createConfigGui(GuiScreen parent)
    {
        GuiConfigs gui = new GuiConfigs();
        gui.setParent(parent);
        return gui;
    }
}

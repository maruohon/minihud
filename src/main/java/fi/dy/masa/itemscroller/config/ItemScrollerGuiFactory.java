package fi.dy.masa.itemscroller.config;

import java.util.Set;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.GuiScreen;
import net.minecraftforge.fml.client.IModGuiFactory;

public class ItemScrollerGuiFactory implements IModGuiFactory
{
    @Override
    public void initialize(Minecraft minecraftInstance)
    {
    }

    @Override
    public Class<? extends GuiScreen> mainConfigGuiClass()
    {
        return ItemScrollerConfigGui.class;
    }

    @Override
    public boolean hasConfigGui()
    {
        return true;
    }

    @Override
    public GuiScreen createConfigGui(GuiScreen parentScreen)
    {
        return new ItemScrollerConfigGui(parentScreen);
    }

    @Override
    public Set<RuntimeOptionCategoryElement> runtimeGuiCategories()
    {
        return null;
    }

    @SuppressWarnings("deprecation")
    @Override
    public RuntimeOptionGuiHandler getHandlerFor(RuntimeOptionCategoryElement element)
    {
        return null;
    }
}

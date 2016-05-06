package fi.dy.masa.minihud.proxy;

import net.minecraft.client.settings.KeyBinding;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.fml.client.registry.ClientRegistry;
import net.minecraftforge.fml.common.FMLCommonHandler;
import fi.dy.masa.minihud.Reference;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.event.InputEventHandler;
import fi.dy.masa.minihud.event.RenderEventHandler;

public class ClientProxy extends CommonProxy
{
    public static KeyBinding keyToggleMode;

    @Override
    public void registerEventHandlers()
    {
        FMLCommonHandler.instance().bus().register(new Configs());
        FMLCommonHandler.instance().bus().register(new InputEventHandler());
        MinecraftForge.EVENT_BUS.register(RenderEventHandler.getInstance());
    }

    @Override
    public void registerKeyBindings()
    {
        keyToggleMode = new KeyBinding(Reference.KEYBIND_NAME_TOGGLE_MODE,
                                       Reference.DEFAULT_KEYBIND_TOGGLE_MODE,
                                       Reference.KEYBIND_CATEGORY_MINIHUD);

        ClientRegistry.registerKeyBinding(keyToggleMode);
    }
}

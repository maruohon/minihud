package fi.dy.masa.minihud.proxy;

import net.minecraft.client.settings.KeyBinding;

import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.fml.client.registry.ClientRegistry;

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
        MinecraftForge.EVENT_BUS.register(new Configs());
        MinecraftForge.EVENT_BUS.register(new InputEventHandler());
        MinecraftForge.EVENT_BUS.register(new RenderEventHandler());
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

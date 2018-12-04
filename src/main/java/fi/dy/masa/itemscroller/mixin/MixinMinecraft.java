package fi.dy.masa.itemscroller.mixin;

import javax.annotation.Nullable;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import fi.dy.masa.itemscroller.event.KeybindCallbacks;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.GuiScreen;
import net.minecraft.client.multiplayer.WorldClient;

@Mixin(Minecraft.class)
public class MixinMinecraft
{
    @Inject(method = "loadWorld(Lnet/minecraft/client/multiplayer/WorldClient;Lnet/minecraft/client/gui/GuiScreen;)V", at = @At("HEAD"))
    private void onLoadWorldPre(@Nullable WorldClient worldClientIn, GuiScreen loadingScreen, CallbackInfo ci)
    {
        if (worldClientIn != null && worldClientIn != (((Minecraft) (Object) this).world))
        {
            KeybindCallbacks.getInstance().onWorldChanged();
        }
    }
}

package fi.dy.masa.minihud.mixin;

import javax.annotation.Nullable;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import fi.dy.masa.minihud.util.DataStorage;
import net.minecraft.client.MinecraftClient;
import net.minecraft.client.gui.Gui;
import net.minecraft.client.world.ClientWorld;

@Mixin(MinecraftClient.class)
public abstract class MixinMinecraftClient
{
    @Inject(method = "method_1550(Lnet/minecraft/client/world/ClientWorld;Lnet/minecraft/client/gui/Gui;)V", at = @At("HEAD"))
    private void onLoadWorldPre(@Nullable ClientWorld worldClientIn, Gui loadingScreen, CallbackInfo ci)
    {
        if (worldClientIn == null || worldClientIn != (((MinecraftClient) (Object) this).world))
        {
            DataStorage.getInstance().reset();
        }
    }
}

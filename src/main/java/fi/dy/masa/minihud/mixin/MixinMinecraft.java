package fi.dy.masa.minihud.mixin;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import fi.dy.masa.minihud.event.InputEventHandler;
import fi.dy.masa.minihud.util.IMinecraftAccessor;
import net.minecraft.client.Minecraft;

@Mixin(Minecraft.class)
public class MixinMinecraft implements IMinecraftAccessor
{
    @Shadow
    private boolean actionKeyF3;

    @Inject(method = "runTickKeyboard", cancellable = true,
            at = @At(value = "INVOKE", target = "Lnet/minecraft/client/Minecraft;dispatchKeypresses()V"))
    public void onKeyboardInput(CallbackInfo ci)
    {
        if (InputEventHandler.getInstance().onKeyInput())
        {
            ci.cancel();
        }
    }

    @Override
    public void setActionKeyF3(boolean value)
    {
        this.actionKeyF3 = value;
    }
}

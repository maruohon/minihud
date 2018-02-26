package fi.dy.masa.minihud.mixin;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import fi.dy.masa.minihud.event.InputEventHandler;
import net.minecraft.client.Minecraft;

@Mixin(Minecraft.class)
public class MixinMinecraft
{
    //@Inject(method = "runTickKeyboard", at = @At(value = "JUMP", opcode = Opcodes.GOTO, ordinal = ??))
    @Inject(method = "dispatchKeypresses", at = @At(value = "HEAD"))
    public void onKeyboardInput(CallbackInfo ci)
    {
        InputEventHandler.getInstance().onKeyInput();
    }
}

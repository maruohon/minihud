package minihud.mixin.info_lines;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

import net.minecraft.client.Minecraft;

import minihud.config.InfoLineToggle;
import minihud.data.DataStorage;

@Mixin(Minecraft.class)
public abstract class MinecraftMixin
{
    @Inject(method = "runTick", at = @At("HEAD"))
    private void onClientTickPre(CallbackInfo ci)
    {
        if (InfoLineToggle.BLOCK_BREAK_SPEED.getBooleanValue())
        {
            DataStorage.getInstance().clearBlockBreakCounter();
        }
    }
}

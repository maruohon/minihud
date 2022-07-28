package fi.dy.masa.minihud.mixin;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;
import net.minecraft.client.gui.screen.ChatScreen;
import net.minecraft.client.gui.screen.Screen;
import net.minecraft.text.Text;
import fi.dy.masa.minihud.util.DataStorage;

@Mixin(ChatScreen.class)
public abstract class MixinChatScreen extends Screen
{
    private MixinChatScreen(Text title)
    {
        super(title);
    }

    @Inject(method = "sendMessage", at = @At("HEAD"), cancellable = true)
    private void onSendChatMessage(String msg, boolean addToHistory, CallbackInfoReturnable<Boolean> cir)
    {
        if (DataStorage.getInstance().onSendChatMessage(msg))
        {
            cir.setReturnValue(true);
        }
    }
}

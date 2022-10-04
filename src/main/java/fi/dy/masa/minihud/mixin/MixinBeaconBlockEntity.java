package fi.dy.masa.minihud.mixin;

import org.objectweb.asm.Opcodes;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import net.minecraft.block.BlockState;
import net.minecraft.block.entity.BeaconBlockEntity;
import net.minecraft.block.entity.BlockEntity;
import net.minecraft.block.entity.BlockEntityType;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import fi.dy.masa.minihud.renderer.OverlayRendererBeaconRange;

@Mixin(BeaconBlockEntity.class)
public abstract class MixinBeaconBlockEntity extends BlockEntity
{
    @Shadow int level;

    private int levelPre;

    private MixinBeaconBlockEntity(BlockEntityType<?> type, BlockPos pos, BlockState state)
    {
        super(type, pos, state);
    }

    @Inject(method = "tick",
            at = @At(value = "INVOKE",
                     target = "Lnet/minecraft/block/entity/BeaconBlockEntity;updateLevel(Lnet/minecraft/world/World;III)I"))
    private static void onUpdateSegmentsPre(World world, BlockPos pos, BlockState state,
                                            BeaconBlockEntity blockEntity, CallbackInfo ci)
    {
        ((MixinBeaconBlockEntity) (Object) blockEntity).levelPre = ((MixinBeaconBlockEntity) (Object) blockEntity).level;
    }

    @Inject(method = "tick",
            at = @At(value = "FIELD", opcode = Opcodes.PUTFIELD, shift = At.Shift.AFTER,
                     target = "Lnet/minecraft/block/entity/BeaconBlockEntity;level:I"))
    private static void onUpdateSegmentsPost(World world, BlockPos pos, BlockState state, BeaconBlockEntity blockEntity, CallbackInfo ci)
    {
        if (((MixinBeaconBlockEntity) (Object) blockEntity).levelPre != ((MixinBeaconBlockEntity) (Object) blockEntity).level)
        {
            OverlayRendererBeaconRange.setNeedsUpdate();
        }
    }
}

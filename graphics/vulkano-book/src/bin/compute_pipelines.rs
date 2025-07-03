use std::sync::Arc;

use vulkano::buffer::{Buffer, BufferContents, BufferCreateInfo, BufferUsage};
use vulkano::command_buffer::allocator::{
    StandardCommandBufferAllocator, StandardCommandBufferAllocatorCreateInfo,
};
use vulkano::command_buffer::{AutoCommandBufferBuilder, CommandBufferUsage};
use vulkano::descriptor_set::allocator::StandardDescriptorSetAllocator;
use vulkano::descriptor_set::{DescriptorSet, WriteDescriptorSet};
use vulkano::device::{Device, DeviceCreateInfo, QueueCreateInfo, QueueFlags};
use vulkano::instance::{Instance, InstanceCreateFlags, InstanceCreateInfo};
use vulkano::memory::allocator::{AllocationCreateInfo, MemoryTypeFilter, StandardMemoryAllocator};
use vulkano::pipeline::compute::ComputePipelineCreateInfo;
use vulkano::pipeline::layout::PipelineDescriptorSetLayoutCreateInfo;
use vulkano::pipeline::{
    ComputePipeline, Pipeline, PipelineBindPoint, PipelineLayout, PipelineShaderStageCreateInfo,
};
use vulkano::sync::GpuFuture;
use vulkano::{VulkanLibrary, sync};

mod cs {
    vulkano_shaders::shader! {
        ty: "compute",
        src: r#"
#version 460

layout(local_size_x = 64, local_size_y = 1, local_size_z = 1) in;

layout(set = 0, binding = 0) buffer Data {
    uint data[];
} buf;

void main() {
    uint index = gl_GlobalInvocationID.x;
    buf.data[index] *= 12;
}
"#,
    }
}

#[derive(BufferContents)]
#[repr(C)]
struct MyStruct {
    a: u32,
    b: u32,
}

fn main() {
    let library = VulkanLibrary::new().expect("no local Vulkan library found");
    let instance = Instance::new(
        library,
        InstanceCreateInfo {
            flags: InstanceCreateFlags::ENUMERATE_PORTABILITY,
            ..Default::default()
        },
    )
    .expect("failed to create instance");

    let physical_device = instance
        .enumerate_physical_devices()
        .expect("failed to enumerate physical devices")
        .next()
        .expect("no physical device found");

    let queue_family_index = physical_device
        .queue_family_properties()
        .iter()
        .position(|query_family_properties| {
            query_family_properties
                .queue_flags
                .contains(QueueFlags::GRAPHICS)
        })
        .expect("no graphics queue family found");

    let (device, mut queues) = Device::new(
        physical_device,
        DeviceCreateInfo {
            queue_create_infos: vec![QueueCreateInfo {
                queue_family_index: queue_family_index as u32,
                ..Default::default()
            }],
            ..Default::default()
        },
    )
    .expect("failed to create device");

    let queue = queues.next().expect("no queue found");

    let memory_allocator = Arc::new(StandardMemoryAllocator::new_default(device.clone()));

    {
        let data_iter = 0..65536u32;
        let data_buffer = Buffer::from_iter(
            memory_allocator.clone(),
            BufferCreateInfo {
                usage: BufferUsage::STORAGE_BUFFER,
                ..Default::default()
            },
            AllocationCreateInfo {
                memory_type_filter: MemoryTypeFilter::PREFER_DEVICE
                    | MemoryTypeFilter::HOST_SEQUENTIAL_WRITE,
                ..Default::default()
            },
            data_iter,
        )
        .expect("failed to create buffer");

        let shader = cs::load(device.clone()).expect("failed to load shader");

        let shader_entry = shader
            .entry_point("main")
            .expect("failed to get entry point");

        let stage = PipelineShaderStageCreateInfo::new(shader_entry);

        let layout = PipelineLayout::new(
            device.clone(),
            PipelineDescriptorSetLayoutCreateInfo::from_stages([&stage])
                .into_pipeline_layout_create_info(device.clone())
                .expect("failed to create pipeline layout"),
        )
        .expect("failed to create pipeline layout");

        let compute_pipeline = ComputePipeline::new(
            device.clone(),
            None,
            ComputePipelineCreateInfo::stage_layout(stage, layout),
        )
        .expect("failed to create compute pipeline");

        let descriptor_set_allocator =
            StandardDescriptorSetAllocator::new(device.clone(), Default::default());

        let pipeline_layout = compute_pipeline.layout();
        let descriptor_set_layouts = pipeline_layout.set_layouts();

        let descriptor_set_layout_index = 0;
        let descriptor_set_layout = descriptor_set_layouts
            .get(descriptor_set_layout_index)
            .expect("failed to get descriptor set layout");

        let descriptor_set = DescriptorSet::new(
            Arc::new(descriptor_set_allocator),
            descriptor_set_layout.clone(),
            [WriteDescriptorSet::buffer(0, data_buffer.clone())],
            [],
        )
        .expect("failed to create descriptor set");

        let command_buffer_allocator = StandardCommandBufferAllocator::new(
            device.clone(),
            StandardCommandBufferAllocatorCreateInfo::default(),
        );

        let mut command_buffer_builder = AutoCommandBufferBuilder::primary(
            Arc::new(command_buffer_allocator),
            queue.queue_family_index(),
            CommandBufferUsage::OneTimeSubmit,
        )
        .expect("failed to create command buffer builder");

        let work_group_counts = [1024, 1, 1];

        unsafe {
            command_buffer_builder
                .bind_pipeline_compute(compute_pipeline.clone())
                .expect("failed to bind pipeline")
                .bind_descriptor_sets(
                    PipelineBindPoint::Compute,
                    compute_pipeline.layout().clone(),
                    descriptor_set_layout_index as u32,
                    descriptor_set,
                )
                .expect("failed to bind descriptor sets")
                .dispatch(work_group_counts)
                .expect("failed to dispatch compute pipeline");
        }

        let command_buffer = command_buffer_builder
            .build()
            .expect("failed to build command buffer");

        sync::now(device.clone())
            .then_execute(queue.clone(), command_buffer)
            .expect("failed to execute command buffer")
            .then_signal_fence_and_flush()
            .expect("failed to signal fence and flush")
            .wait(None)
            .expect("failed to wait for fence");

        let content = data_buffer.read().expect("failed to read from buffer");

        for (n, val) in content.iter().enumerate() {
            assert_eq!(*val, (n as u32) * 12);
        }

        println!("Everything successful!");
    }
}

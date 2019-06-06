using System.Reflection;
using Autofac;
using MediatR;

namespace LearnLibs.Tests
{
    class MediatRUtils
    {
        public static IContainer BuildMediatRContainer()
        {
            var builder = new ContainerBuilder();
            builder.RegisterType<Mediator>()
                .As<IMediator>()
                .InstancePerLifetimeScope();
            builder.Register<ServiceFactory>(context =>
            {
                var c = context.Resolve<IComponentContext>();
                return t => c.Resolve(t);
            });
            builder.RegisterAssemblyTypes(typeof(MediatRUtils).GetTypeInfo().Assembly).AsImplementedInterfaces();
            return builder.Build();
        }

        public static IMediator BuildMediator()
        {
            var container = BuildMediatRContainer();
            return container.Resolve<IMediator>();
        }
    }
}

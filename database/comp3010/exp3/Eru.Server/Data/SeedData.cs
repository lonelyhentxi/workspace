using System;
using System.Collections.Generic;
using System.Linq;
using Eru.Server.Data.Models;
using Microsoft.AspNetCore.Identity;
using Microsoft.AspNetCore.Mvc.ModelBinding;
using Microsoft.EntityFrameworkCore;
using Microsoft.Extensions.DependencyInjection;

namespace Eru.Server.Data
{
    public static class SeedData
    {
        public static void Initialize(IServiceProvider serviceProvider,string type)
        {
            using (var context = new EruContext(serviceProvider.GetRequiredService<DbContextOptions<EruContext>>()))
            {
                if (context.Users.Any(u=>u.Name=="master"))
                {
                    return;
                }

                using (var transaction = context.Database.BeginTransaction())
                {
                    Action<string> setIdentityInsertOn = (string tableName) =>
                    {
                        var prefix = "";
                        if (type == "PostgreSQL")
                        {
                            tableName = tableName.ToLower();
                        }
                        if (type == "SqlServer")
                        {
                            prefix = "dbo.";
                        }
                        try
                        {
                            context.Database.ExecuteSqlCommand("SET IDENTITY_INSERT "+prefix + tableName + " ON;");
                        }
                        catch (Exception)
                        {
                            //Console.WriteLine(e);
                        }
                    };
                    Action<string> setIdentityInsertOff = (string tableName) =>
                    {
                        var prefix = "";
                        if (type == "SqlServer")
                        {
                            prefix = "dbo.";
                        }

                        if (type == "PostgreSQL")
                        {
                            tableName = tableName.ToLower();
                        }

                        try
                        {
                            context.Database.ExecuteSqlCommand("SET IDENTITY_INSERT "+prefix + tableName + " OFF;");
                        }
                        catch (Exception)
                        {
                            //Console.WriteLine(e);
                        }
                    };

                    var passwordHasher = new PasswordHasher<string>();
                    var now = DateTime.Now;
                    var masterGuid = Guid.Parse("2a2eec6f-58f5-44e1-95e8-887955c17533");
                    context.Users.AddRange(new User()
                    {
                        Id = masterGuid,
                        Name = "master",
                        Password = passwordHasher.HashPassword(masterGuid.ToString(), "master"),
                        Description = "blog master",
                        Avatar = "https://avatars3.githubusercontent.com/u/23011677?s=460&v=4",
                        Url = "https://github.com/lonelyhentai",
                        CreateTime = now,
                        UpdateTime = now
                    });

                    context.UserProfiles.AddRange(new UserProfile()
                    {
                        Id = masterGuid,
                        Misc = "{}",
                        Setting = "{}",
                    });
                    context.Roles.AddRange(
                        new Role
                        {
                            Id =1,
                            Description = "Administration",
                            Name = "admin"
                        },
                        new Role
                        {
                            Id=2,
                            Description = "Author",
                            Name = "author",
                        },
                        new Role
                        {
                            Id =3,
                            Description = "User",
                            Name = "user"
                        });
                    setIdentityInsertOn("Roles");
                    context.SaveChanges();
                    setIdentityInsertOff("Roles");

                    // {WR}/{LEVEL}/{SCOPE}/{RESOURCE}
                    var permissions = new Permission[]
                    {
                        new Permission
                        {
                            Id =1,
                            Description = "write all resource",
                            Name = "w/a/a/all", // write/abstract/all/all
                        },
                        new Permission
                        {
                            Id =2,
                            Description = "read all resource detailed",
                            Name = "w/d/a/all" // write/detail/all/all
                        },
                        new Permission
                        {
                            Id = 3,
                            Description = "read all resource",
                            Name = "r/a/a/all" // read/abstract/all/all
                        },
                        new Permission
                        {
                            Id = 4,
                            Description = "read self resource detailed",
                            Name = "r/d/s/all"
                        },
                        new Permission
                        {
                            Id = 5,
                            Description = "write self user detailed",
                            Name = "w/d/s/user"
                        },
                        new Permission
                        {
                            Id =6,
                            Description = "write self blog",
                            Name = "w/_/s/blog",
                        },
                        new Permission
                        {
                            Id =7,
                            Description = "write self comment",
                            Name = "w/_/s/comment"
                        }
                    };
                    context.Permissions.AddRange(
                        permissions
                    );
                    var rolePermissions = permissions
                        .Select(p => new RolePermissionAssociation {RoleId = 1, PermissionId = p.Id}).ToList();
                    rolePermissions.AddRange(permissions
                        .Where(p => p.Id == 3 || p.Id == 4 || p.Id == 5 || p.Id == 6 || p.Id == 7)
                        .Select(p => new RolePermissionAssociation {RoleId = 2, PermissionId = p.Id}).ToList());
                    rolePermissions.AddRange(permissions
                        .Where(p => p.Id == 3 || p.Id == 4 || p.Id == 6 || p.Id == 7)
                        .Select(p => new RolePermissionAssociation {RoleId = 3, PermissionId = p.Id}).ToList());
                    context.RolePermissionAssociations.AddRange(rolePermissions);
                    context.UserRoleAssociations.AddRange(new UserRoleAssociation[]
                    {
                        new UserRoleAssociation() {UserId = masterGuid, RoleId = 1},
                        new UserRoleAssociation() {UserId = masterGuid, RoleId = 2},
                        new UserRoleAssociation() {UserId = masterGuid, RoleId = 3},
                    });

                    
                    
                    setIdentityInsertOn("Permissions");
                    context.SaveChanges();
                    setIdentityInsertOff("Permissions");

                    var postCategories = new PostCategory[]
                    {
                        new PostCategory
                        {
                            Id = 1,
                            Name = "技术",
                            Description = "技术宅改变世界"
                        },
                        new PostCategory {
                            Id = 2,
                            Name = "日常",
                            Description = "日常也能如此有趣"
                        },
                        new PostCategory
                        {
                            Id = 3,
                            Name = "欢乐",
                            Description = "沙雕人人爱"
                        },
                        new PostCategory
                        {
                            Id = 4,
                            Name = "娱乐",
                            Description = "玩是人类的天性",
                        },
                        new PostCategory
                        {
                            Id = 5,
                            Name = "转载",
                            Description = "转载的事，不能叫……"
                        }
                    };
                    context.PostCategories.AddRange(postCategories);
                    setIdentityInsertOn("PostCategories");
                    context.SaveChanges();
                    setIdentityInsertOff("PostCategories");


                    var applications = new Application []
                    {
                        new Application
                        {
                            Name = "日程",
                            Avatar = "bg-error text-white calendar",
                            Description = "日程",
                            CreateTime = now,
                            UpdateTime = now,
                            Id = Guid.Parse("81fa5d1a-1e23-4214-9e8d-a1a2e8ce01a5"),
                            Url = "",
                            Version = "0.0.1"
                        },
                        new Application
                        {
                            Name = "笔记",
                            Avatar = "bg-geekblue text-white file",
                            Description = "笔记",
                            CreateTime = now,
                            UpdateTime = now,
                            Id = Guid.Parse("9658bcc6-2eb5-43ff-bbb1-bcbb881e78ab"),
                            Url = "",
                            Version = "0.0.1"
                        },
                        new Application
                        {
                            Name = "同步",
                            Avatar = "bg-success text-white cloud",
                            Description = "同步",
                            CreateTime = now,
                            UpdateTime = now,
                            Id = Guid.Parse("fe71b3bc-33f6-4c7a-9a60-44ed063376f8"),
                            Url = "",
                            Version = "0.0.1"
                        },
                        new Application
                        {
                            Name = "收藏",
                            Avatar = "bg-magenta text-white anticon-star",
                            Description = "收藏",
                            CreateTime = now,
                            UpdateTime = now,
                            Id = Guid.Parse("efc1cf28-5581-4a24-9a64-cc992c3ad37e"),
                            Url = "",
                            Version = "0.0.1"
                        },
                        new Application
                        {
                            Name = "项目管理",
                            Avatar = "bg-purple text-white team",
                            Description = "项目管理",
                            CreateTime = now,
                            UpdateTime = now,
                            Id = Guid.Parse("e4f49bb8-fda8-4c3e-8ff0-495ced3f8c93"),
                            Url = "",
                            Version = "0.0.1"
                        },
                        new Application
                        {
                            Name = "工具",
                            Avatar = "bg-warning text-white scan",
                            Description = "工具",
                            CreateTime = now,
                            UpdateTime = now,
                            Id = Guid.Parse("ed5d52e9-1a8e-4c2a-8620-88b04d8f6d8c"),
                            Url = "",
                            Version = "0.0.1"
                        },
                        new Application
                        {
                            Name = "赞助",
                            Avatar = "bg-cyan text-white pay-circle outline",
                            Description = "赞助",
                            CreateTime = now,
                            UpdateTime = now,
                            Id = Guid.Parse("927bdce0-9e65-4bca-9b12-d6a4fdf15b1d"),
                            Url = "",
                            Version = "0.0.1"
                        },
                        new Application
                        {
                            Name = "生产力",
                            Avatar = "bg-grey text-white printer",
                            Description = "生产力",
                            CreateTime = now,
                            UpdateTime = now,
                            Id = Guid.Parse("321357af-b8d8-4b5f-aef8-c3803ce9860d"),
                            Url = "",
                            Version = "0.0.1"
                        }
                    };
                    context.Applications.AddRange(applications);
                    context.ApplicationProfiles.AddRange(applications.Select(a=>new ApplicationProfile {Id=a.Id,Misc = "{}",Setting = "{}"}));
                    context.SaveChanges();
                    var postStatuses = new PostStatus[]
                    {
                        new PostStatus
                        {
                            Id = 1,
                            Name = "已发布",
                            Description = "在早期版本中这是唯一的状态"
                        }
                    };
                    context.PostStatuses.AddRange(postStatuses);
                    setIdentityInsertOn("PostStatuses");
                    context.SaveChanges();
                    setIdentityInsertOff("PostStatuses");

                    var postTags = new PostTag[]
                    {
                        new PostTag
                        {
                            Id = 1,
                            Name = "Eru",
                            Description = "与 Eru 博客有关"
                        },
                        new PostTag
                        {
                            Id = 2,
                            Name = "开发",
                            Description = "开发相关"
                        },
                        new PostTag
                        {
                            Id = 3,
                            Name = "测试",
                            Description = "测试相关"
                        }
                    };
                    context.PostTags.AddRange(postTags);
                    setIdentityInsertOn("PostTags");
                    context.SaveChanges();
                    setIdentityInsertOff("PostTags");

                    var posts = new Post[]
                    {
                        new Post
                        {
                            Id = Guid.Parse("f91149fc-1cad-4587-a19d-1ea2d90058cb"),
                            Title = "这是第一篇文章",
                            Description = "Eru 博客引擎的第一篇测试文章",
                            Content = "Eru 博客引擎的第一篇测试文章 - 留作纪念",
                            CategoryId = 1,
                            CreateTime = now,
                            UpdateTime = now,
                            UserId = Guid.Parse("2a2eec6f-58f5-44e1-95e8-887955c17533"),
                            StatusId = 1,
                        },
                        new Post
                        {
                            Id = Guid.Parse("cf8331c4-8655-4e7f-8a1c-7018f7a8ccaf"),
                            Title = "分类测试文章",
                            Description = "分类测试文章",
                            Content = "分类测试文章",
                            CategoryId = 2,
                            CreateTime = now.AddDays(1),
                            UpdateTime = now.AddDays(1),
                            UserId = Guid.Parse("2a2eec6f-58f5-44e1-95e8-887955c17533"),
                            StatusId = 1,
                        },
                        new Post
                        {
                            Id = Guid.Parse("9fc03c5e-a526-47e6-9243-3cc943ee3d6c"),
                            Title = "AI 芯片调研报告",
                            Content = "# AI 芯片调研报告\r\n\r\n\r\n<!-- @import \"[TOC]\" {cmd=\"toc\" depthFrom=1 depthTo=6 orderedList=false} -->\r\n\r\n<!-- code_chunk_output -->\r\n\r\n* [AI 芯片调研报告](#ai-芯片调研报告)\r\n\t* [一. 研究内容](#一-研究内容)\r\n\t* [二. 摘要](#二-摘要)\r\n\t* [三. 介绍](#三-介绍)\r\n\t\t* [3.1 研究的目的与动机](#31-研究的目的与动机)\r\n\t\t* [3.2 本研究贡献和组织说明](#32-本研究贡献和组织说明)\r\n\t* [四. 相关调研](#四-相关调研)\r\n\t\t* [4.1 相关新闻报道](#41-相关新闻报道)\r\n\t\t* [4.2 相关研究论文](#42-相关研究论文)\r\n\t\t\t* [4.2.1 功耗控制](#421-功耗控制)\r\n\t\t\t* [4.2.2 同架构多目的设计](#422-同架构多目的设计)\r\n\t\t\t* [4.2.3 容错性改进](#423-容错性改进)\r\n\t\t\t* [4.2.4 异步设计](#424-异步设计)\r\n\t\t\t* [4.2.5 存储集成](#425-存储集成)\r\n\t\t\t* [4.2.6 低精度推断](#426-低精度推断)\r\n\t\t\t* [4.2.7 稀疏化](#427-稀疏化)\r\n\t\t\t* [4.2.8 规模的挑战](#428-规模的挑战)\r\n\t\t\t* [4.2.9 软硬件配套](#429-软硬件配套)\r\n\t\t* [4.3 相关项目与研究团队](#43-相关项目与研究团队)\r\n\t\t\t* [4.3.1 英伟达 GPU](#431-英伟达-gpu)\r\n\t\t\t* [4.3.2 谷歌 TPU](#432-谷歌-tpu)\r\n\t\t\t* [4.3.3 英特尔 VPU](#433-英特尔-vpu)\r\n\t\t\t* [4.3.4 开源 VTA](#434-开源-vta)\r\n\t\t\t* [4.3.5 寒武纪 NPU](#435-寒武纪-npu)\r\n\t\t\t* [4.3.6 百度 XPU](#436-百度-xpu)\r\n\t\t\t* [4.3.7 深鉴科技 DPU](#437-深鉴科技-dpu)\r\n\t\t\t* [4.3.8 比特大陆 SOPHON](#438-比特大陆-sophon)\r\n\t\t\t* [4.3.9 其它在研中的项目和团队](#439-其它在研中的项目和团队)\r\n\t* [五. AI 芯片常见架构](#五-ai-芯片常见架构)\r\n\t\t* [5.1 硬件框架](#51-硬件框架)\r\n\t\t\t* [5.1.1 通用处理器芯片 GPP](#511-通用处理器芯片-gpp)\r\n\t\t\t* [5.1.2 专用处理器芯片 ASP](#512-专用处理器芯片-asp)\r\n\t\t\t* [5.1.3 可配置的硬件芯片 Configurable Hardware](#513-可配置的硬件芯片-configurable-hardware)\r\n\t\t\t* [5.1.4 协处理器芯片 Co-Processors](#514-协处理器芯片-co-processors)\r\n\t\t\t* [5.1.5 专用集成电路 ASIC](#515-专用集成电路-asic)\r\n\t\t\t* [5.1.6 类脑芯片](#516-类脑芯片)\r\n\t\t* [5.2 软件框架](#52-软件框架)\r\n\t\t\t* [5.2.1 tensorflow](#521-tensorflow)\r\n\t\t\t* [5.2.2 keras](#522-keras)\r\n\t\t\t* [5.2.3 pytorch + caffe2](#523-pytorch-caffe2)\r\n\t\t\t* [5.2.4 Theano](#524-theano)\r\n\t\t\t* [5.2.5 MXNet](#525-mxnet)\r\n\t\t\t* [5.2.6 CNTK](#526-cntk)\r\n\t* [六. 问题讨论](#六-问题讨论)\r\n\t\t* [6.1 AI芯片适合的应用](#61-ai芯片适合的应用)\r\n\t\t* [6.2 与GPU架构比较：优点与缺点](#62-与gpu架构比较优点与缺点)\r\n\t\t\t* [6.2.1 优点](#621-优点)\r\n\t\t\t* [6.2.2 缺点](#622-缺点)\r\n\t\t* [6.3 AI芯片的问题和限制](#63-ai芯片的问题和限制)\r\n\t\t\t* [6.3.1 功耗问题](#631-功耗问题)\r\n\t\t\t* [6.3.2 同构问题](#632-同构问题)\r\n\t\t\t* [6.3.3 规模问题](#633-规模问题)\r\n\t\t\t* [6.3.4 AI 特性](#634-ai-特性)\r\n\t\t\t* [6.3.5 软硬件配套问题](#635-软硬件配套问题)\r\n\t* [七. 例子实作流程](#七-例子实作流程)\r\n\t\t* [7.1 AI 芯片如何整合人工智能框架](#71-ai-芯片如何整合人工智能框架)\r\n\t\t\t* [7.1.1 IP Kernel 及其分组](#711-ip-kernel-及其分组)\r\n\t\t\t* [7.1.2 中间层的形成](#712-中间层的形成)\r\n\t\t\t* [7.1.3 高层级支持](#713-高层级支持)\r\n\t\t* [7.2 AI 芯片如何在Xilinx上开发](#72-ai-芯片如何在xilinx上开发)\r\n\t\t\t* [7.2.1 Xilinx Machine Learning Suite](#721-xilinx-machine-learning-suite)\r\n\t\t\t* [7.2.2 底层开发，自建生态](#722-底层开发自建生态)\r\n\t\t\t* [八. 总结与感想](#八-总结与感想)\r\n\r\n<!-- /code_chunk_output -->\r\n\r\n## 一. 研究内容\r\n\r\nAI 芯片研究报告\r\n\r\n## 二. 摘要\r\n\r\n随着 AI 产业快速突破，极大的计算力和集成度的需求促生了 AI 芯片需求的诞生和产业趋势的出现，研究 AI 芯片成为 AI 领域和半导体领域共同的研究热点和重点。本文以 AI 芯片为切入点和中心，回顾相关产业的历史、总结现状、分析几种方式和趋势的利弊，并对相关领域的未来做出一定的展望。\r\n\r\n## 三. 介绍\r\n\r\n### 3.1 研究的目的与动机\r\n\r\n人类或许是始终怀揣着成为造物主的梦想。自首台计算机的设想被提出以来，自动化的终极理想 —— 人工智能在方向各异的数次学术浪潮的主导下，几度繁华，又多次落寞。进入新时代，以机器学习、尤其是神经网络为主导的联结主义在计算力的指数增长下，卷土重来。\r\n\r\n业界乃至社会各界，无论是高举旗帜的、还是坚定质疑的，无一不怀着极大的热情，投身或是被裹挟着进入这一新的浪潮之中。\r\n\r\n在同一时期，大规模集成电路行业也有过它样的沉浮。自上世纪下半叶第一块集成电路被制出依赖，有过仙童的叱咤风云、八十年代日厂的如日中天、世纪末的台韩崛起；然而，正所谓天下大势，分久必合，合久必分，在纷乱的春秋争霸之后，终于在二十一世纪初叶形成了台积电、三星、英特尔、格罗方德几超几强的战国军阀割据格局。\r\n\r\n正是处在这样的格局之中。随着摩尔定律寿命的临近，产业研发的投入产出的“边际成本”越来越大，后来的新生势力在传统领域内与传统豪强的竞争几乎已经是天方夜谭。只能在产业低端出卖血汗，苟且偷生的新生集团，在努力维持存续、追逐先发者的同时，也时刻在觊觎着先发者的丰厚利润，紧盯着弯道超车的机会。\r\n\r\n这时的电子学术界，抛除国内被材料主导的那一拨外，承接计算机领域提出的新热点，在 ASIC 化之前，通过 FPGA 实现热点核心算法，成为诸多研究人员批量生产论文的，屡试不爽的法宝。\r\n\r\n或许是时也命也；又或许是冥冥之中，已经注定。这样几波浪潮的高潮在一条以十数年为单位的时间轴上，发生密切度极高的重合，产生了相当奇妙的化学反应。AI 计算的专业化， GPU 化到 FPGA 化，乃至，某些算法的透彻研究之后 —— 像区块链的加密技术那样被 ASIC 化。专用和半专用 AI 芯片动辄提高数十上百倍的性能、降低数倍的功耗、时间无不令从业人员垂涎；同时，以AI芯片方面为突破口的，也是我国集成电路产业提高商品竞争力，实现弯道追赶、乃至超车的有力途径。无论最终高度如何，都将为中国制造2025、在电子和信息技术领域添加重要基石。\r\n\r\n本次研究工作的主要目的和动机正在于此，当然，会回顾相关产业的历史、总结现状、分析几种方式和趋势的利弊，并对相关领域的未来做出一定的展望。\r\n\r\n### 3.2 本研究贡献和组织说明\r\n\r\n本次研究由作者周烨恒独立完成。主要参考的会议有 ISSCC（2016、2017、2018），SIGGRAPH（2018）；主要资料的来源有 github、gitbook、google 学术以及本文直接或者间接指出的各企业、高校和研究机构网站；主要媒体有 infoq，google news，quora，知乎，以及本文中涉及的各媒体网站。\r\n\r\n特别鸣谢计算机设计与实践课的各位老师，在课上和课下社交群组讨论中，对于在计算机芯片设计和AI芯片设计方面的引导、点拨和视角参考。\r\n\r\n## 四. 相关调研\r\n\r\n### 4.1 相关新闻报道\r\n\r\n- Nvidia's AI to Power Daimler Robotaxi Fleet by 2020. - TheStreet.com\r\n- 华为首款手机端AI芯片麒麟970！ — 机器之心\r\n- 寒武纪获1亿美元融资 中国诞生AI芯片首个独角兽。— 中金在线\r\n- Facebook reportedly hires AI chip head from Google - Zacks.com\r\n- Google announces Edge TPU development kit - bit-tech.net\r\n- Xilinx Buys DeePhi Technology to Slim Down Machine Learning - Electronic Design\r\n- ……\r\n\r\n### 4.2 相关研究论文\r\n\r\n#### 4.2.1 功耗控制\r\n\r\n- 《A 2.9TOPS/W Deep Convolutional Neural Network SoC in FD-SOI 28nm for Intelligent Embedded Systems》- ISSCC 2017 14.1，《ENVISION: A 0.26-to-10TOPS/W Subword-Parallel Dynamic-Voltage-Accuracy-Frequency-Scalable Convolutional Neural Network Processor in 28nm FDSOI》- ISSCC 2017 14.5\r\n\r\n使用低功耗工艺。\r\n\r\n- 《DNPU: An 8.1TOPS/W Reconfigurable CNN-RNN Processor for General-Purpose Deep Neural Networks》- ISSCC 2017 14.2\r\n\r\n优化乘法，降低 weight 量化位数，降低数据计算功耗。\r\n\r\n- 《A 28nm SoC with a 1.2GHz 568nJ/Prediction Sparse Deep-Neural-Network Engine with >0.1 Timing Error Rate Tolerance for IoT Applications》- ISSCC 2017 14.3\r\n\r\n降低数据翻转功耗。\r\n\r\n- 《A Scalable Speech Recognizer with Deep-Neural-Network Acoustic Models and Voice-Activated Power Gating》- ISSCC 2017 14.4\r\n\r\n选择模型时考虑性耗比。\r\n\r\n- 《A 0.62mW Ultra-Low-Power Convolutional-Neural-Network Face-Recognition Processor and a CIS Integrated with Always-On Haar-Like Face Detector》- ISSCC 2017 14.6 , 《A 288μW Programmable Deep-Learning Processor with 270KB On-Chip Weight Storage Using Non-Uniform Memory Hierarchy for Mobile Intelligence》ISSCC 2017 14.7\r\n\r\n降低数据存储访问功耗。\r\n\r\n#### 4.2.2 同架构多目的设计\r\n\r\n- 《DNPU: An 8.1TOPS/W Reconfigurable CNN-RNN Processor for General-Purpose Deep Neural Networks》 - ISSCC 2017 14.2\r\n\r\n通过以下方法，实现同一架构对 CNN 和 RNN 的高效处理：\r\n\r\n1. 在片上存储受限制的情况下减少片上存储的访问。\r\n2. 适应不同层的不同数据分布，动态范围。\r\n3. 将 16bit weight 转化为 4bit index， 将乘法转化为查表。 \r\n\r\n#### 4.2.3 容错性改进\r\n\r\n- 《A 28nm SoC with a 1.2GHz 568nJ/Prediction Sparse Deep-Neural-Network Engine with >0.1 Timing Error Rate Tolerance for IoT Applications》- ISSCC 2017 14.3\r\n\r\n充分利用网络内部的容错性和 Razor、time borrowing 等方法，放宽电路中关键路径的约束，允许一定数量的错误。利用 ML/DL 应用和算法层面的并行性，伸缩性，稀疏性和容错性，来提高硬件的效率，降低硬件的功耗。\r\n\r\n#### 4.2.4 异步设计\r\n\r\n- 《Petascale Deep Learning on a Single Chip》- Vathys\r\n\r\n提高异步电路中的控制电路的水平。\r\n\r\n#### 4.2.5 存储集成\r\n\r\n- 《QUEST: A 7.49TOPS Multi-Purpose Log-Quantized DNN Inference Engine Stacked on 96MB 3D SRAM Using Inductive-Coupling Technology in 40nm CMOS》 - ISSCC 2018 13.2\r\n\r\n巨大的片上 Memory Cell，无线 SRAM 堆叠技术。 该项技术能给 AI 芯片的存储集成带来新的选项。其工作集成了SRAM。优势在于读写速度等。\r\n\r\n这篇论文就实现了3-Cycle Uniform Random R/W Latency。对于架构设计者来说，很多设计都是为了应对DRAM和片上存储在读写速度上的差异。所以，这种新的封装模式又给了架构设计者一个新的工具，可以有更多Tradeoff空间。\r\n\r\n#### 4.2.6 低精度推断\r\n\r\n - 《UNPU: A 50.6TOPS/W Unified Deep Neural Network Accelerator with 1b-to-16b Fully-Variable Weight Bit-Precision》- ISSCC 2018 13.3，《QUEST: A 7.49TOPS Multi-Purpose Log-Quantized DNN Inference Engine Stacked on 96MB 3D SRAM Using Inductive-Coupling Technology in 40nm CMOS》 - ISSCC 2018 13.2\r\n\r\n随着当下多精度推断逐渐支持以 edge/embedded 端为代表的实际应用，其实现已经逐渐成为现在硬件设计的一个趋势和重点。主要难度是对于不同的应用和算法，精度要求不同，必须能够在一个架构上支持不同精度的处理。\r\n\r\n上述提到的两篇文章，都有在支持从1bit-16bit不同精度的工作。\r\n\r\n![low bit inference](low_bit_inference.jpg)\r\n\r\n#### 4.2.7 稀疏化\r\n\r\n- 《Eyeriss: An Energy-Efficient Reconfigurable Accelerator for Deep Convolutional Neural Networks》- ISSCC 2016 14.7\r\n\r\n对于 feature：压缩。将稀疏化的 feature 压缩后存储到外部DRAM内；对于 weight：遇到 0 值参与的乘法运算时，跳过乘法计算，跳过 weight 参数的读取。\r\n\r\n#### 4.2.8 规模的挑战\r\n\r\n- 《Even at the Edge, Scale is the Real Challenge》 Nvidia\r\n\r\n以自动驾驶领域为例，讨论了在神经网络的数据和运算规模不断增加的背景下，对于大规模并行处理架构的挑战。基于一定的估计，进一步分析了，如何通过multi-GPU（Nvlink），multi-node connection（InfiniBand），以及flash storage or flash-accelerated storage来实现一个规模化，且可扩展的架构，以应对计算和存储上的挑战。\r\n\r\n![scale challenge](scale_challenge.jpg)\r\n\r\n#### 4.2.9 软硬件配套\r\n\r\n- 《VTA: Deep Learning Accelerator Stack》- [vta docs](https://docs.tvm.ai/vta/index.html)\r\n\r\n对于 AI 芯片而言，软件硬件的结合已经成为诸多企业和高校研发的阻碍。硬件产品在真正的使用中面临的一系列软件方面的需求和压力，成为了让我们的项目完成的一道大大的鸿沟。\r\n\r\n深度学习芯片从来不只是硬件的问题，而是涉及到一个从硬件，到驱动，到编译优化部署，再到模型本身的全栈问题。\r\n\r\n### 4.3 相关项目与研究团队\r\n\r\n#### 4.3.1 英伟达 GPU\r\n\r\n如果问在 AI 芯片领域，当前涉及面最广、市场份额最大的公司是谁，想必大多数的从业者和研发人员会给出相同的结论 —— 英伟达。\r\n\r\n早年在游戏玩家群体中早已出名的 GTX 系列显卡，近来又成为机器学习的新宠。在公司的大力支持下，不但有新的人工智能优化的 Titan V 等型号的推出；而且 cuda、cudnn 等基础设施更已代替 opencl 等，几乎成为事实标准；\r\n\r\n一方面，英伟达保持自己原来在 GPU 上固有的优势，而另一方面，又试图取得新的突破，不仅涉足自动驾驶汽车、高性能计算、机器人、医疗保健、云计算、游戏视频等众多领域，更是针对自动驾驶汽车领域，推出了全新人工智能超级计算机 Xavier。\r\n\r\n#### 4.3.2 谷歌 TPU\r\n\r\n众所周知，谷歌是这一波 AI 浪潮的引领者，不仅其软件框架 Tensorflow 在开发者群体中早已人人皆知，其以 AlphaGo 为代表的相关产品，更是已经在公众水平上做到了妇孺皆知。相关联的，随着不断增长的 AI 相关的计算能力的需求，谷歌正当时宜的提出自己的异构计算系统 TPU —— 张量处理单元（ Tensor Processing Unit ）。\r\n\r\n此后的几年中，谷歌不断更新着自己的产品设计，加深了人工智能在学习和推理方面的能力。据称，其 TPU 比同时代的 GPU 节省一半训练时间。\r\n\r\n第二代TPU包括了四个芯片，每秒可处理180万亿次浮点运算；如果将64个TPU组合到一起，升级为所谓的TPU Pods，则可提供大约11500万亿次浮点运算能力。\r\n\r\n#### 4.3.3 英特尔 VPU\r\n\r\n长期以来，英特尔是世界上最大的计算机芯片制造商，但是随着近些年来个人消费市场的增长乏力，也一直在寻求着传统计算机以外的市场。在一次次尝试中，英特尔有过 Phi 这样的类 GPU 产品，但是市场表现都远不能称得上是成功。\r\n\r\n为了加强在 AI 等方面的实力，英特尔不仅斥巨资收购了 FPGA 生产商 Altera 公司，还收购自动驾驶技术公司 Mobileye ，机器视觉公司Movidius和为自动驾驶汽车芯片提供安全工具的公司Yogitech 等。\r\n\r\nMyriad X 就是英特尔子公司 Movidius 在 2017 年推出的视觉处理器（ VPU，vision processing unit），这是一款低功耗的系统芯片，用于在基于视觉的设备上加速深度学习和人工智能。\r\n\r\n#### 4.3.4 开源 VTA\r\n\r\nAI 芯片和硬件加速是深度学习发展写下一个大的方向，而如何设计专用加速芯片无疑是大家都感兴趣的话题。然而，深度学习芯片的主要问题远远超过了硬件本身。对于每个深度学习模型，如果要设计加速器，我们都设计一套关联的驱动，以及上面的软件栈。\r\n\r\nVTA，Versatile Tensor Accelerator，直译为灵活的张量加速器。首先，VTA 是一个完全开源的深度学习加速器。但是 VTA 不光包含了加速器设计本身，完整的驱动，tvm 编译的整合和直接从tvm 前端 python 编译部署深度学习模型的完整开源工具链。\r\n\r\nVTA 试图解决的问题是：如何降低AI芯片设计门槛，让每一个人都可以玩深度学习硬件加速和系统开发。\r\n\r\n对于硬件设计者而言，VTA就像一个硬件和软件栈的蓝图。对于深度学习的从业人员来说，VTA的设计总结了现在的深度学习加速器的特性，并且会不断更新反应最新的研究成果。系统设计者可以通过VTA为目标来完成优化深度学习加速器的一个基石。\r\n\r\n#### 4.3.5 寒武纪 NPU\r\n\r\n寒武纪作为业界的新生力量，却彰显除了非凡的实力和野望。在众多研发团队仍然处于实验阶段的时候，寒武纪 1a NPU 芯片就早已量产，作为华为麒麟 970 的协处理器风光了一回。\r\n\r\n寒武纪是世界顶尖的智能处理器研发团队，研发了全球首个深度学习专用处理器原型芯片、全球首个智能处理器指令集、全球首个商用深度学习专用处理器，全球首个MLU智能芯片，与中科曙光合作推出全球首款基于寒武纪芯片的AI推理专用服务器Phaneron。同样，如前所述的，为全球首个人工智能手机芯片华为麒麟970（Kirin 970）提供了强大的人工智能处理能力。 \r\n\r\n据称，目前寒武纪设立了三条产品线：首先是智能终端处理器IP授权，智能IP指令集可授权集成到手机、安防、可穿戴设备等终端芯片中，客户包括国内顶尖SoC厂商，目前已经开始投入市场。而2016年全年就已拿到1个亿元订单。这也使得寒武纪研发了国际首个深度学习专用处理器芯片，于2016年第一年成立，就实现盈利。\r\n\r\n#### 4.3.6 百度 XPU\r\n\r\n过去几年，百度在深度学习领域，尤其是基于GPU的深度学习领域取得了不错的进展。刚刚在加州Hot Chips大会上，百度发布XPU，这是一款256核、基于FPGA的云计算加速芯片。合作伙伴是赛思灵（Xilinx）。\r\n\r\n百度研究员欧阳剑表示，百度设计的芯片架构突出多样性，着重于计算密集型、基于规则的任务，同时确保效率、性能和灵活性的最大化。XPU的目标是在性能和效率之间实现平衡，并处理多样化的计算任务。\r\n\r\n#### 4.3.7 深鉴科技 DPU\r\n\r\nDNNDK (Deep Neural Network Development Kit)是深鉴科技面向AI异构计算平台DPU（Deep-Learning Processor Unit，深度学习处理器）自主研发的原创深度学习SDK，为DPU平台各种深度学习应用开发和部署提供的一套高效全栈式解决方案。\r\n\r\n深鉴科技作为一家初创企业，其优势在于神经网络编译器和深度压缩工具等。它们或者相比同类产品具有较大的性能优势，或者业界\r\n暂无同类产品。\r\n\r\n据报道，深鉴科技于 2018 年 7 月 17 日被全球最大的 FPGA 公司赛灵思收购。 \r\n\r\n#### 4.3.8 比特大陆 SOPHON\r\n\r\n作为一家出身于数字货币芯片和设备的公司，同时也是世界上最大的比特币矿机芯片公司，比特大陆早已不满足 “屈居” 数字货币设备的一隅，涉足了 AI 芯片的领域。\r\n\r\n比特大陆2017年11月份推出的首款AI芯片被命名为Sophon BM1680。主打的产品有：\r\n\r\n- 张量计算处理器；\r\n- 深度学习加速卡；\r\n- 智能视频分析服务器；\r\n\r\n#### 4.3.9 其它在研中的项目和团队\r\n\r\n- 阿里达摩院 ALI-NPU\r\n- 地平线 BPU\r\n- 眼擎科技\r\n- MIT 人工突触研究团队\r\n- ……\r\n\r\n## 五. AI 芯片常见架构\r\n\r\n### 5.1 硬件框架\r\n\r\n![computing choice](computing_choice.jpg)\r\n\r\n图片来自 RIT 的 Shaaban 教授\r\n\r\n#### 5.1.1 通用处理器芯片 GPP\r\n\r\n通用处理器芯片，General Purpose Processors，GPP。例如 CPU，ARM 等。\r\n\r\n其长处在于泛用性，该类芯片完全依赖于通用编程模型，适应于传统的软件编写模式，灵活性非常高。\r\n\r\n但是，该类芯片对特定用途的计算优化不足，导致如果只是长期进行某类单一用途或者特定领域的应用和算法的时候，计算力、功耗等都不占据优势。\r\n\r\n当前市场上，AI 芯片基本都不是通用处理器芯片。使用通用处理器芯片，主要是应对某些思路、逻辑等都和以往算法截然不同，或者是无法利用当前领域特化芯片优势的任务。该类硬件架构主要由传统芯片霸主把持。\r\n\r\n#### 5.1.2 专用处理器芯片 ASP\r\n\r\n专用处理器芯片，Application-Specific Processors，ASP。常见的该类芯片包括：图形处理器 GPU，领域特化处理器 DSP，网络处理器 NP，媒体处理器，向量处理器 等。\r\n\r\n该类处理器的特点是，他们都是针对某些较大的应用领域特化设计的，泛用性能（例如分支深度等）可能不够，但是在专属的领域的并行性能，可能数倍甚至数十倍于通用处理器芯片。同时，相较于特化程度更高芯片，该类芯片又保持了一定的灵活性，配套的软件栈也相对容易使用和拓展。\r\n\r\n当前该类架构的代表是英伟达的 GTX 系列以及更高端的 Titan 系列 GPU。被广泛地使用在世界各地的高效和企业中，平衡性价比和灵活度、能耗所带来总体优势较大，生态环境也很好。\r\n\r\n#### 5.1.3 可配置的硬件芯片 Configurable Hardware\r\n\r\n可配置的硬件芯片，Configurable Hardware。该类芯片在当下当之无愧的代表是现场可编程门阵列 FPGA。\r\n\r\n该类芯片是一种半定制专用集成电路。它通过软件手段更改、配置器件内部连接结构和逻辑单元，完成既定功能设计。\r\n\r\n相对于专用处理器芯片，它的可配置性更强，用户可以在不重新设计、流片的基础上，一方面获得对硬件逻辑高效的把控能力，充分地针对应用类型进行器件内部连接和逻辑单元的优化设计，另一方面相对传统专用集成电路，高效地适用算法。\r\n\r\n但是其对硬件知识的要求高于 GPP 和 ASP，开发效率也远不如它们。该类型 AI 芯片适用于对性能和功耗都有较高要求，但是又需要一定灵活性的应用。\r\n \r\n#### 5.1.4 协处理器芯片 Co-Processors\r\n\r\n协处理器芯片，Co-Processors，CP。这是一种协助中央处理器完成其无法执行或执行效率、效果低下的处理工作而开发和应用的处理器。它们的任务类型非常特定，因为其它不擅长的任务都已经交由 GPP 处理，所以调度等功能也都可以简化，因此功耗可以很低。同时它们在处理自己的特化的任务时，同样有极高的性耗比。\r\n\r\n该类架构的代表是谷歌的 TPU，它有很多 DSA 组成，每一个DSA处理单元都有负责的独特领域并针对该领域做优化，当计算机系统遇到相关计算时便由相应的DSA处理器去负责。\r\n\r\n#### 5.1.5 专用集成电路 ASIC\r\n\r\n专用集成电路，ASIC。该类型在硬件层面上固化了算法和应用逻辑，非常适用于高度特定化并且已经固定的算法流程，在对应的逻辑上性能远高于其它任意类型的架构，并且一旦产量足够，成本也远低于其它类型。但是，该类芯片灵活性极低，调整微小的逻辑都需要重新设计流片，还需要有相关的硬件、软件配套开发人才。\r\n\r\n#### 5.1.6 类脑芯片\r\n\r\n类脑芯片，顾名思义，该类芯片是通过在各种特性上高度模仿生物学意义上的大脑，尤其是其神经突触和连接，与通过开关信号计算的数字电路非常不同，其元件以模拟方式进行工作，通过交换梯度信号或权重信号来激活，非常类似神经元依靠流过突触的离子种类和数量来激活。\r\n\r\n但是由于生物学研究的不透彻和制造工艺等方面的不足，以及该类架构本身的复杂性，它远不如以上几类那么成熟。\r\n\r\n### 5.2 软件框架\r\n\r\n#### 5.2.1 tensorflow\r\n\r\nTensorFlow是谷歌开发的用于深度学习或人工神经网络的开源软件库。它由Google AI团队内的Google Brain团队开发，许可证为Apache 2.0。TensorFlow使用数据流图进行数值计算。它具有非常灵活的体系结构，可以轻松部署跨各种平台（CPU，GPU，TPU）以及从台式机到服务器群集到移动和边缘设备的计算。\r\n\r\n#### 5.2.2 keras\r\n\r\n用Python编写的Keras是一个开源库，旨在简化新DL模型的创建。这种高级神经网络API可以在TensorFlow，Microsoft CNTK等深度学习框架之上运行。该工具以其用户友好性和模块化而闻名，因而非常适合快速原型开发。该工具针对CPU和GPU进行了优化。\r\n\r\n#### 5.2.3 pytorch + caffe2\r\n\r\ncaffe2 是一个轻量级、模块化、可伸缩的深度学习框架；pytorch 是一个快速、灵活的深度学习框架。近期，它们的 github 官方代码库已经合并。\r\n\r\n#### 5.2.4 Theano\r\n\r\nTheano是一个专为深度学习而设计的Python库。它允许你定义、优化和评估涉及高效率的多维数组的数学表达式。它可以使用GPU并执行有效的符号区分。该工具具有与NumPy集成、动态C代码生成和符号区分等功能。\r\n\r\n#### 5.2.5 MXNet\r\n\r\nMXNet 是亚马逊选择的深度学习库。它拥有数据流图，为多 GPU 配置提供了良好的配置，和高级别的模型构建块，并且可以在你可以想象的任何硬件上运行（包括手机）。对 Python 的支持只是其冰山一角—MXNet 同样提供了对 R、Julia、C++、Scala、Matlab，和 Javascript 的接口。\r\n\r\n#### 5.2.6 CNTK\r\n\r\n微软认知工具是由微软研究院开发的深度学习框架，旨在使用神经网络来处理非结构化数据的大型数据集。CNTK具高度的可定制性，因为它具有更快的训练时间和易于使用的体系结构。无论是在只有CPU，单个GPU，多个GPU或具有多个GPU的多个机器上运行，它都拥有出色的性能。\r\n\r\n## 六. 问题讨论\r\n\r\n### 6.1 AI芯片适合的应用\r\n\r\n在图像识别等领域，常用的是CNN卷积网络，语音识别、自然语言处理等领域，主要是RNN，这是两类有区别的算法。但是，他们本质上，都是矩阵或vector的乘法、加法，然后配合一些除法、指数等算法。而对于这些流程相对固定，只是需要极大规模的并行算力的算法步骤，将其固化为硬件逻辑，有助于提高效率，降低功耗。\r\n\r\n### 6.2 与GPU架构比较：优点与缺点\r\n\r\n#### 6.2.1 优点\r\n\r\n1. 针对对应 AI 算法和应用设计，速度快、性能高。\r\n2. 删减了很多不必要的功能，降低了功耗、成本。\r\n\r\n#### 6.2.2 缺点\r\n\r\n1. 灵活性低、通用性低，只能适用于一类的应用，进化困难。\r\n2. 建立软件栈、生态链困难。\r\n\r\n### 6.3 AI芯片的问题和限制\r\n\r\n#### 6.3.1 功耗问题\r\n\r\n1. 如何在端芯片保持计算能力的同时，降低功耗，以维持设备续航。\r\n2. 如何提高集群计算芯片的单位功耗的计算能力。\r\n\r\n#### 6.3.2 同构问题\r\n\r\n针对不同的任务，适合的机器学习算法会不一样。比如，各种network的构造都不一样，而且还有CNN、RNN这种区别。开发一个AI芯片，不可能只适用于一种结构的network。都希望在一个AI芯片上能跑多种网络结构，如resnet，alexnet，inception等等。\r\n\r\n#### 6.3.3 规模问题\r\n\r\n在提升计算能力时，单位性能提升成本随计算能力的提高而提高，工艺难度的提升。\r\n\r\n#### 6.3.4 AI 特性\r\n\r\nAI 领域知识和算法更新很快，芯片设计如何跟上这种速度？\r\n\r\n#### 6.3.5 软硬件配套问题\r\n\r\n开发 AI 芯片还需要部署相对应的编译、链接、调试等软件栈，还需要开发生态环境。对无论高校还是企业，都是较大的挑战。\r\n\r\n## 七. 例子实作流程\r\n\r\n### 7.1 AI 芯片如何整合人工智能框架\r\n\r\n#### 7.1.1 IP Kernel 及其分组\r\n\r\n对人工智能框架的基本数据结构，如 tensorflow 的 tensor，caffe 的 Blob，MxNet 的 NDArray 做硬件上的实例化，并将这些单元按照框架常见分组级别进行分组，形成很多组 IP Kernel，其附带有自己的控制组件、存储组件、设置组件。\r\n\r\n并且形成的 IP Kernel 大多数是为了通用的 AI 计算设计的（如矩阵乘法等），还有一些特化的单元，应对特殊用途。这些 IP Kernel 的预设有对不同目的的分组方式和结构、数量（例如针对小型模型的分组等），并且依照分组优化 Kernel 间连接。\r\n\r\n这一层级的 IP Kernel 和 分组的设计有很多可以参考 NVIDIA 公司在其显卡中的 Cuda 单元和其分组的设计。\r\n\r\n#### 7.1.2 中间层的形成\r\n\r\n中间层是用来将底层的 IP Kernel 和分组按照功能、结构、阶段等组织成功能性、层级性和区域性 API 而设计的。\r\n\r\n该层应该提供一系列低粒度、良好定义、高性能的控制、数据桥梁，以在中高层的基础设施软件栈、机器学习框架和底层的 IP Kernel 及分组之间进行沟通。\r\n\r\n该层还应该封装一些常用的功能，例如和 master CPU 之间的交互。 \r\n\r\n#### 7.1.3 高层级支持\r\n\r\n用常用高级语言开发基于 Restful 、RPC 风格 HTTP协议或者语言内调用的 API（如python）和常用框架进行交互，并将命令通过中间层转发给底层，底层将结果发送给高级语言等层面。与外界的交互一方面可以由该层 API 支持，另一方面可以由框架或者框架支持。实际上后者更好，因为框架本身支持的 API 语言本来较多。\r\n\r\n### 7.2 AI 芯片如何在Xilinx上开发\r\n\r\n#### 7.2.1 Xilinx Machine Learning Suite \r\n\r\nXilinx 官方提供了 [Xilinx Machine Learning Suite](https://github.com/xilinx/ml-suite)。它为用户提供了开发和部署实时推断的机器学习应用的工具，它还为很多种常用的机器学习框架（诸如 MxNet、Caffe、Tensorflow）提供了 Python 和 Restful 风格的接口。\r\n\r\n![Xilinx Machine Learning Suite](xilinx_ml_suite.png)\r\n\r\n该套件主要由以下三个部分组成：\r\n\r\n1. xDNN IP - 高性能的通用 CNN 处理引擎\r\n2. xfDNN Middleware - 提供给机器学习框架和优化实时推断的软件库和工具接口。\r\n3. ML Framework and Open Source Support 为高级机器学框架和开源项目提供的支持。\r\n\r\n#### 7.2.2 底层开发，自建生态\r\n\r\n将 AI 算法的通用部分和高频率部分抽象后，用 HDL 语言实现，生成硬件逻辑，将需配置和调参的部分暴露交给中间层，中间层使用高级语言包装成领域专属语言或者当下常用开发语言和框架的接口提交给用户。\r\n\r\n实质上，整体的思路就是仿照 Xilinx Machine Learning Suite 重新造一遍轮子。\r\n\r\n## 八. 总结与感想\r\n\r\n通过本次研究，我们看到在 AI 芯片领域，既有传统 AI 和半导体问题的延续，又有新的问题和挑战的提出。\r\n\r\n我们在端侧不断的追求能耗比的提升，在云端又追求性能、规模和可拓展性。与此同时，硬件架构的复杂和多样性、软件设计和开发的匹配度、复杂性不断为这个产业带来新的挑战。\r\n\r\n各种新的设想、设计不断提出，各种巧夺天工、别出心裁的工艺推陈出新，推动着整个业界不断前往新的高度。每一个参与者都要不断拓展视野的广度和技术的深度，紧跟时代的潮流，把握前进的方向，才能勇做 AI 芯片大潮的弄潮儿。",
                            Description = "计算机组成与实践课程作业",
                            StatusId = 1,
                            CategoryId = 1,
                            CreateTime = now.AddDays(2),
                            UpdateTime = now.AddDays(2),
                            UserId = Guid.Parse("2a2eec6f-58f5-44e1-95e8-887955c17533")
                        }
                    };
                    context.Posts.AddRange(posts);
                    context.SaveChanges();

                    context.PostTagAssociations.AddRange(new PostTagAssociation[]
                    {
                        new PostTagAssociation
                        {
                            PostId = Guid.Parse("f91149fc-1cad-4587-a19d-1ea2d90058cb"),
                            TagId = 3,
                        },
                        new PostTagAssociation
                        {
                            PostId = Guid.Parse("f91149fc-1cad-4587-a19d-1ea2d90058cb"),
                            TagId = 2,
                        },
                        new PostTagAssociation {
                            PostId = Guid.Parse("f91149fc-1cad-4587-a19d-1ea2d90058cb"),
                            TagId = 1,
                        },
                        new PostTagAssociation
                        {
                        PostId = Guid.Parse("cf8331c4-8655-4e7f-8a1c-7018f7a8ccaf"),
                        TagId = 3,
                        },
                        new PostTagAssociation
                        {
                            PostId = Guid.Parse("cf8331c4-8655-4e7f-8a1c-7018f7a8ccaf"),
                            TagId = 2,
                        },
                        new PostTagAssociation
                        {
                            PostId = Guid.Parse("cf8331c4-8655-4e7f-8a1c-7018f7a8ccaf"),
                            TagId = 1,
                        },
                        new PostTagAssociation {
                            PostId = Guid.Parse("9fc03c5e-a526-47e6-9243-3cc943ee3d6c"),
                            TagId = 2,
                        }
                    });

                    context.SaveChanges();

                    context.CommentCategories.Add(new CommentCategory
                    {
                        Id = 1,
                        Name = "普通评论",
                        Description = "早期版本默认的评论方式"
                    });

                    setIdentityInsertOn("CommentCategories");
                    context.SaveChanges();
                    setIdentityInsertOff("CommentCategories");

                    context.CommentStatuses.Add(new CommentStatus()  
                    {
                        Id = 1,
                        Name = "已发布",
                        Description = "早期版本默认的评论状态"
                    });

                    setIdentityInsertOn("CommentStatuses");
                    context.SaveChanges();
                    setIdentityInsertOff("CommentStatuses");

                    var comments = new List<Comment> { };
                    for (var i = 0; i < 10; i++)
                    {
                        comments.Add(new Comment
                        {
                            Id = Guid.NewGuid(),
                            CategoryId = 1,
                            StatusId =1,
                            CreateTime = now.AddHours(i),
                            UpdateTime = now.AddHours(i),
                            Content = "抢到" + i + "楼",
                            UserId = masterGuid,
                            PostId = Guid.Parse("f91149fc-1cad-4587-a19d-1ea2d90058cb"),
                        });
                        if (i % 5 == 0&&i!=0)
                        {
                            comments[comments.Count - 1].ParentId = comments[comments.Count - 6].Id;
                        }
                    }

                    context.AddRange(comments);
                    context.SaveChanges();
                    transaction.Commit();
                }
            }
        }
    }
}
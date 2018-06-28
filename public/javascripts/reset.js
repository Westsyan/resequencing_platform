
var htmlpe = "<select class=\"form-control\" name=\"adapter\" id=\"adapter\">\n" +
    "                                                <option value=\"TruSeq2-PE.fa\">\n" +
    "                                                    TruSeq2 (paired-ended, for Illumina GAII)</option>\n" +
    "                                                <option value=\"TruSeq3-PE.fa\">\n" +
    "                                                    TruSeq3 (paired-ended, for MiSeq and HiSeq)</option>\n" +
    "                                                <option value=\"TruSeq3-PE-2.fa\">\n" +
    "                                                    TruSeq3 (additional seqs) (paired-ended, for MiSeq and HiSeq)</option>\n" +
    "                                                <option value=\"NexteraPE-PE.fa\" id=\"pe4\">Nextera (paired-ended)</option>\n" +
    "                                            </select>";

var htmlse = "<select class=\"form-control\" name=\"adapter\" id=\"adapter\">\n" +
    "                                                <option value=\"TruSeq2-SE.fa\">\n" +
    "                                                    TruSeq2 (single-ended, for Illumina GAII)</option>\n" +
    "                                                <option value=\"TruSeq3-SE.fa\">\n" +
    "                                                    TruSeq3 (single-ended, for MiSeq and HiSeq)</option>\n" +
    "                                            </select>"

function restart(obj) {
    var id = obj.value;
    $.ajax({
        url: "/resequencing/sample/deployGet?id=" + id,
        type: "get",
        dataType: "json",
        success: function (data) {
            $("#selectAdapter").empty();
            if (data.inputType == "PE") {
                $("#aas").show();
                $("#aos").show();
                $("#ans").show();
                $("#selectAdapter").append(htmlpe);
            } else {
                $("#aas").hide();
                $("#aos").hide();
                $("#ans").hide();
                $("#selectAdapter").append(htmlse);
            }
            hideArgue();
            $("#peId").val(data.id);
            $("#proname").val(data.proname);
            $("#sample").val(data.sample);
            $("#encondingType").val(data.encondingType);
            $("#stepMethod").val(data.stepMethod);
            $("#adapter").val(data.adapter);
            $("#seed_mismatches").val(data.seed_mismatches);
            $("#palindrome_clip_threshold").val(data.palindrome_clip_threshold);
            $("#simple_clip_threshold").val(data.simple_clip_threshold);
            $("#trimMethod").val(data.trimMethod);
            $("#window_size").val(data.window_size);
            $("#required_quality").val(data.required_quality);
            $("#minlenMethod").val(data.minlenMethod);
            $("#minlen").val(data.minlen);
            $("#leadingMethod").val(data.leadingMethod);
            $("#leading").val(data.leading);
            $("#trailingMethod").val(data.trailingMethod);
            $("#trailing").val(data.trailing);
            $("#cropMethod").val(data.cropMethod);
            $("#crop").val(data.crop);
            $("#headcropMethod").val(data.headcropMethod);
            $("#headcrop").val(data.headcrop);
            $("#adv_pe_options_selector").val(data.adv_pe_options_selector);
            $("#aa").val(data.aa);
            $("#ao").val(data.ao);
            $("#an").val(data.an);
            $("#aN").val(data.aN);
            $("#analysis_type_selector").val(data.analysis_type_selector);
            $("#n").val(data.n);
            $("#o").val(data.o);
            $("#e").val(data.e);
            $("#i").val(data.i);
            $("#d").val(data.d);
            $("#l").val(data.l);
            $("#k").val(data.k);
            $("#m").val(data.m);
            $("#M").val(data.M);
            $("#O").val(data.O);
            $("#E").val(data.E);
            $("#R").val(data.R);
            $("#q").val(data.q);
            $("#L").val(data.L);
            $("#mapq").val(data.mapq);
            $("#restPE").modal("show");
        }
    })
}




$("#down-1").click(function () {
    $("#set-1").show();
    $("#down-1").hide();
    $("#up-1").show()
});

$("#up-1").click(function () {
    $("#set-1").hide();
    $("#down-1").show();
    $("#up-1").hide()
});

$("#down-2").click(function () {
    $("#set-2").show();
    $("#down-2").hide();
    $("#up-2").show()
});

$("#up-2").click(function () {
    $("#set-2").hide();
    $("#down-2").show();
    $("#up-2").hide()
});

function stepChange(element) {
    var value = $(element).find(">option:selected").val()
    if (value == "yes") {
        $("#stepValue").show()
    } else {
        $("#stepValue").hide()
    }
}

function trimChange(element) {
    var value = $(element).find(">option:selected").val()
    if (value == "yes") {
        $("#trimValue").show()
    } else {
        $("#trimValue").hide()
    }
}

function minlenChange(element) {
    var value = $(element).find(">option:selected").val()
    if (value == "yes") {
        $("#minlenValue").show()
    } else {
        $("#minlenValue").hide()
    }
}

function leadingChange(element) {
    var value = $(element).find(">option:selected").val()
    if (value == "yes") {
        $("#leadingValue").show()
    } else {
        $("#leadingValue").hide()
    }
}

function trailingChange(element) {
    var value = $(element).find(">option:selected").val()
    if (value == "yes") {
        $("#trailingValue").show()
    } else {
        $("#trailingValue").hide()
    }
}

function cropChange(element) {
    var value = $(element).find(">option:selected").val()
    if (value == "yes") {
        $("#cropValue").show()
    } else {
        $("#cropValue").hide()
    }
}

function headcropChange(element) {
    var value = $(element).find(">option:selected").val()
    if (value == "yes") {
        $("#headcropValue").show()
    } else {
        $("#headcropValue").hide()
    }
}

function advChange(element) {
    var value = $(element).find(">option:selected").val()
    if (value == "set") {
        $("#advValue").show()
    } else {
        $("#advValue").hide()
    }
}

function anaChange(element) {
    var value = $(element).find(">option:selected").val()
    if (value == "full") {
        $("#anaValue").show()
    } else {
        $("#anaValue").hide()
    }
}


function RunningPE() {
    var form = $("#resetPEForm")
    var fv = form.data("formValidation")
    fv.validate()
    if (fv.isValid()) {
        $.ajax({
            url: "/resequencing/sample/resetPE",
            type: "post",
            dataType: "json",
            data: $("#resetPEForm").serialize(),
            success: function (data) {
                if (data.valid == "true") {
                    $("#restPE").modal("hide");
                    updateTable();
                    var id = $("#peId").val();
                    $.ajax({
                        url: "/resequencing/sample/isRunCmd?id=" + id,
                        type: "post"
                    })
                }
            }
        })
    }
}


function formValidation() {
    $('#resetPEForm').formValidation({
        framework: 'bootstrap',
        icon: {
            valid: 'glyphicon glyphicon-ok',
            invalid: 'glyphicon glyphicon-remove',
            validating: 'glyphicon glyphicon-refresh'
        },
        fields: {
            seed_mismatches: {
                validators: {
                    notEmpty: {
                        message: '不能为空！'
                    },
                    integer: {
                        message: '必须为整数！'
                    }
                }
            },
            palindrome_clip_threshold: {
                validators: {
                    notEmpty: {
                        message: '不能为空！'
                    },
                    integer: {
                        message: '必须为整数！'
                    }
                }
            },
            simple_clip_threshold: {
                validators: {
                    notEmpty: {
                        message: '不能为空！'
                    },
                    integer: {
                        message: '必须为整数！'
                    }
                }
            },
            window_size: {
                validators: {
                    notEmpty: {
                        message: '不能为空！'
                    },
                    integer: {
                        message: '必须为整数！'
                    }
                }
            },
            required_quality: {
                validators: {
                    notEmpty: {
                        message: '不能为空！'
                    },
                    integer: {
                        message: '必须为整数！'
                    }
                }
            },
            minlen: {
                validators: {
                    notEmpty: {
                        message: '不能为空！'
                    },
                    integer: {
                        message: '必须为整数！'
                    }
                }
            },
            leading: {
                validators: {
                    notEmpty: {
                        message: '不能为空！'
                    },
                    integer: {
                        message: '必须为整数！'
                    }
                }
            },
            trailing: {
                validators: {
                    notEmpty: {
                        message: '不能为空！'
                    },
                    integer: {
                        message: '必须为整数！'
                    }
                }
            },
            crop: {
                validators: {
                    notEmpty: {
                        message: '不能为空！'
                    },
                    integer: {
                        message: '必须为整数！'
                    }
                }
            },
            headcrop: {
                validators: {
                    notEmpty: {
                        message: '不能为空！'
                    },
                    integer: {
                        message: '必须为整数！'
                    }
                }
            },
            aa: {
                validators: {
                    notEmpty: {
                        message: '不能为空！'
                    },
                    integer: {
                        message: '必须为整数！'
                    }
                }
            },
            ao: {
                validators: {
                    notEmpty: {
                        message: '不能为空！'
                    },
                    integer: {
                        message: '必须为整数！'
                    }
                }
            },
            an: {
                validators: {
                    notEmpty: {
                        message: '不能为空！'
                    },
                    integer: {
                        message: '必须为整数！'
                    }
                }
            },
            aN: {
                validators: {
                    notEmpty: {
                        message: '不能为空！'
                    },
                    integer: {
                        message: '必须为整数！'
                    }
                }
            },
            n: {
                validators: {
                    notEmpty: {
                        message: '不能为空！'
                    },
                    numeric: {
                        message: '必须为整数！'
                    }
                }
            },
            o: {
                validators: {
                    notEmpty: {
                        message: '不能为空！'
                    },
                    integer: {
                        message: '必须为整数！'
                    }
                }
            },
            e: {
                validators: {
                    notEmpty: {
                        message: '不能为空！'
                    },
                    integer: {
                        message: '必须为整数！'
                    }
                }
            },
            i: {
                validators: {
                    notEmpty: {
                        message: '不能为空！'
                    },
                    integer: {
                        message: '必须为整数！'
                    }
                }
            },
            d: {
                validators: {
                    notEmpty: {
                        message: '不能为空！'
                    },
                    integer: {
                        message: '必须为整数！'
                    }
                }
            },
            l: {
                validators: {
                    notEmpty: {
                        message: '不能为空！'
                    },
                    integer: {
                        message: '必须为整数！'
                    }
                }
            },
            k: {
                validators: {
                    notEmpty: {
                        message: '不能为空！'
                    },
                    integer: {
                        message: '必须为整数！'
                    }
                }
            },
            m: {
                validators: {
                    notEmpty: {
                        message: '不能为空！'
                    },
                    integer: {
                        message: '必须为整数！'
                    }
                }
            },
            M: {
                validators: {
                    notEmpty: {
                        message: '不能为空！'
                    },
                    integer: {
                        message: '必须为整数！'
                    }
                }
            },
            O: {
                validators: {
                    notEmpty: {
                        message: '不能为空！'
                    },
                    integer: {
                        message: '必须为整数！'
                    }
                }
            },
            E: {
                validators: {
                    notEmpty: {
                        message: '不能为空！'
                    },
                    integer: {
                        message: '必须为整数！'
                    }
                }
            },
            R: {
                validators: {
                    notEmpty: {
                        message: '不能为空！'
                    },
                    integer: {
                        message: '必须为整数！'
                    }
                }
            },
            q: {
                validators: {
                    notEmpty: {
                        message: '不能为空！'
                    },
                    integer: {
                        message: '必须为整数！'
                    }
                }
            },
            L: {
                validators: {
                    notEmpty: {
                        message: '不能为空！'
                    },
                    integer: {
                        message: '必须为整数！'
                    }
                }
            }
        }
    })
}




